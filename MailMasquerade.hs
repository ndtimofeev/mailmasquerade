{-# LANGUAGE MultiWayIf, FlexibleInstances, StandaloneDeriving, Strict, TypeOperators, DataKinds, DeriveTraversable, OverloadedStrings, DeriveGeneric #-}

module MailMasquerade where

import Control.Applicative
import Control.Exception
import Control.Lens hiding ( Wrapped, Unwrapped )
import Control.Monad
import qualified Control.Monad.State as MS
import Data.Aeson (FromJSON, ToJSON, toEncoding)
import qualified Data.Aeson as JSON
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.IMF as PB
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.MIME as PB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL (connectIMAPSSL)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.SMTP.Internal
import Network.HaskellNet.SMTP.SSL (doSMTPSSL)
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Options.Generic
import qualified System.IO as IO
import System.Directory
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler.Syslog

-- a map from Message-IDs to sender addresses
-- populated by every message coming from the outside world
type ReplyDB = Map ByteString ByteString


data Arguments f = Arguments
	{ verbose :: f ::: Bool   <#> "v" <?> "More logs"
	, debug   :: f ::: Bool   <#> "d" <?> "Even more logs than -v, includes full raw mail headers (sensitive)"
	, stdout  :: f ::: Bool   <#> "s" <?> "Put log to stdout only"
	, config  :: f ::: String <#> "c" <?> "Path to config file"
	} deriving Generic


instance ParseRecord (Arguments Wrapped)


deriving instance Show (Arguments Unwrapped)


data Config mail = Config
	{ imapServer     :: String
	, smtpServer     :: String
	, username       :: mail
	, password       :: String
	, target         :: mail
	, whitelist      :: [mail]
	, defaultReplyTo :: [mail]	-- addresses to send the mail to when we're unable to figure who the target is replying to
	} deriving (Show, Generic, Functor, Foldable, Traversable)


instance FromJSON a => FromJSON (Config a) where


instance ToJSON a => ToJSON (Config a) where
	toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- logger namespaces, one per subsystem
logConfig, logIMAP, logSMTP, logMail, logPing, logReplyDB :: String
logConfig  = "mailmasquerade.config"
logIMAP    = "mailmasquerade.imap"
logSMTP    = "mailmasquerade.smtp"
logMail    = "mailmasquerade.mail"
logPing    = "mailmasquerade.mail.ping"
logReplyDB = "mailmasquerade.replydb"


main :: IO ()
main = do
	args <- unwrapRecord "MailMasquerade"
	let level
		| debug args   = DEBUG
		| verbose args = INFO
		| otherwise    = WARNING
	-- start from a clean slate: hslogger's root logger comes with a stderr
	-- handler by default, and we want exactly one destination, not stderr
	-- plus whatever we add below
	updateGlobalLogger rootLoggerName (setLevel level . removeHandler)
	if stdout args
		then do
			h <- streamHandler IO.stdout level
			updateGlobalLogger rootLoggerName (addHandler h)
		else do
			s <- openlog "mailmasquerade" [PID] USER level
			updateGlobalLogger rootLoggerName (addHandler s)
	infoM logConfig $ "Log level set to " ++ show level
	bs   <- B.readFile (config args)
	let conf' = either error id $ JSON.eitherDecodeStrict bs >>= traverse (PB.parse (mailboxToSpec <$> PB.mailbox PB.defaultCharsets) . Data.ByteString.Lazy.Char8.pack)
	-- conf <- either error id <$> JSON.eitherDecodeFileStrict (config args)
	infoM logConfig $ "Opened configuration file " ++ config args
	fetchMail conf'


specToString :: PB.AddrSpec -> String
specToString spec  = Data.ByteString.Char8.unpack $ PB.renderAddressSpec spec


specToAddress :: PB.AddrSpec -> Address
specToAddress spec = Address { addressName = Nothing, addressEmail = T.decodeLatin1 $ PB.renderAddressSpec spec }


adjustMailReply :: PB.Message ctx a -> PB.AddrSpec -> PB.AddrSpec -> PB.Message ctx a
adjustMailReply msg from to = flip MS.execState msg $ do
	msgids <- use PB.headerInReplyTo
	modifying PB.headerReferences (\refs -> L.nub (msgids ++ refs))
	modifying (PB.headerReplyTo PB.defaultCharsets) (const [])
	modifying (PB.headerFrom PB.defaultCharsets) (refrom from)
	modifying (PB.headerTo PB.defaultCharsets) (const [PB.Single (PB.Mailbox Nothing to)])


adjustMailForForwarding :: PB.Message ctx a -> PB.AddrSpec -> PB.AddrSpec -> PB.Message ctx a
adjustMailForForwarding msg from to = flip MS.execState msg $ do
	modifying (PB.headerFrom PB.defaultCharsets) (refrom from)
	modifying (PB.headerTo PB.defaultCharsets) (const [PB.Single (PB.Mailbox Nothing to)])


refrom :: PB.AddrSpec -> [PB.Address] -> [PB.Address]
refrom from xs = case xs of
	[PB.Single (PB.Mailbox mval _)] -> [PB.Single (PB.Mailbox mval from)]
	_                               -> [PB.Single (PB.Mailbox Nothing from)]


getFromAddrs :: PB.Message ctx a -> [PB.AddrSpec]
getFromAddrs mail = concatMap addrToSpec $ view (PB.headerFrom PB.defaultCharsets) mail


isPing :: PB.Message ctx a -> Bool
isPing mail = maybe False ((== "ping") . Text.toCaseFold . Text.strip)
	$ view (PB.headerSubject PB.defaultCharsets) mail


tossMail :: Config PB.AddrSpec -> BL.ByteString -> Address -> IO ()
tossMail conf mail to = handle logSendFailure $ doSMTPSSL (smtpServer conf) $ \conn -> do
	authSuccess <- SMTP.authenticate PLAIN (specToString $ username conf) (password conf) conn
	when (not authSuccess) $ error "authentication failed"
	sendMailData (specToAddress $ username conf) [to] (BL.toStrict mail) conn
	infoM logSMTP $ "Sent mail to " ++ show (addressEmail to)
	where
	logSendFailure e = do
		errorM logSMTP $ "Failed to send mail to " ++ show (addressEmail to) ++ ": " ++ show (e :: SomeException)
		throwIO e


fetchMail :: Config PB.AddrSpec -> IO ()
fetchMail conf = do
	forever $ handle (\e -> errorM logIMAP $ "IMAP session failed: " ++ show (e :: SomeException)) $ do
		conn <- connectIMAPSSL (imapServer conf)
		infoM logIMAP $ "Connected to " ++ imapServer conf
		login conn (specToString $ username conf) (password conf)
		infoM logIMAP $ "Logged in as " ++ specToString (username conf)
		forever $ do
			grabNewMail conf conn
			idle conn $ 1000 * 60 * 29	-- rfc9051
			debugM logIMAP $ "IDLE returned, checking for new mail"


grabNewMail :: Config PB.AddrSpec -> IMAPConnection -> IO ()
grabNewMail conf conn = do
	select conn "INBOX"
	msgs <- search conn [UNFLAG Seen]
	debugM logIMAP $ "Unseen message IDs: " ++ show msgs
	forM_ msgs (fetch conn >=> handleNewMail conf)


addrToSpec :: PB.Address -> [PB.AddrSpec]
addrToSpec addr = case addr of
	PB.Single mbox   -> [mailboxToSpec mbox]
	PB.Group _ mboxs -> map mailboxToSpec mboxs


mailboxToSpec :: PB.Mailbox -> PB.AddrSpec
mailboxToSpec (PB.Mailbox _ spec) = spec


handleNewMail :: Config PB.AddrSpec -> ByteString -> IO ()
handleNewMail conf mail = do
	case PB.parse (PB.message PB.mime) mail of
		Left e -> errorM logMail $ "Failed to parse incoming mail: " ++ show e
		Right parsedMail@(PB.Message (PB.Headers hdrs) _) -> do
			let fromAddrs = getFromAddrs parsedMail
			    msgid     = maybe "?" (Data.ByteString.Char8.unpack . PB.renderMessageID) $ view PB.headerMessageID parsedMail
			    logMsg lvl msg = lvl logMail $ "[" ++ msgid ++ "] " ++ msg
			logMsg infoM $ "Handling new mail from " ++ show fromAddrs
			logMsg debugM $ unlines $ map show hdrs
			if	| Just spec <- listToMaybe fromAddrs
				, spec `elem` whitelist conf
				, isPing parsedMail -> do
					infoM logPing $ "[" ++ msgid ++ "] PING from " ++ specToString spec ++ ", ponging back"
					let pong = set (PB.headerSubject PB.defaultCharsets) (Just "PONG")
						     $ adjustMailForForwarding parsedMail (username conf) spec
					tossMail conf (PB.renderMessage pong) $ specToAddress spec

				| target conf `elem` fromAddrs -> do
					logMsg infoM "Remote mail from target, looking up reply address"
					maddr <- replyDBFetch parsedMail
					let sendTo = maybe (defaultReplyTo conf) pure maddr
					logMsg infoM $ "Replying to " ++ show sendTo
					forM_ sendTo $ \addr -> do
						let newMail@(PB.Message (PB.Headers rewrittenHdrs) _) = adjustMailReply parsedMail (username conf) addr
						logMsg debugM $ "Rewritten headers for " ++ specToString addr ++ ": " ++ unlines (map show rewrittenHdrs)
						tossMail conf (PB.renderMessage newMail) $ specToAddress addr

				| Just spec <- listToMaybe fromAddrs
				, spec `elem` whitelist conf -> do
					logMsg infoM $ "Local mail from whitelisted " ++ specToString spec ++ ", forwarding to " ++ specToString (target conf)
					tossMail conf (PB.renderMessage $ adjustMailForForwarding parsedMail (username conf) (target conf)) $ specToAddress $ target conf
					replyDBAdd parsedMail

				| otherwise -> logMsg debugM "From neither target nor whitelist, dropping"


replyDBFile = "replydb.bin"
replyDBTemporaryFile = "replydb.bin.tmp"
replyDBBackupFile = "replydb.bin.bak"


-- creates an empty database on read failure
replyDBRead :: IO ReplyDB
replyDBRead = catch (do
		replyDBBinary <- BL.readFile replyDBFile
		pure $ decode replyDBBinary
	) $ \e -> do
		warningM logReplyDB $ "Could not read " ++ replyDBFile ++ ", starting with an empty reply database: " ++ show (e :: SomeException)
		pure mempty


replyDBWrite :: ReplyDB -> IO ()
replyDBWrite replyDB = do
	BL.writeFile replyDBTemporaryFile $ encode replyDB
	catch (renameFile replyDBFile replyDBBackupFile) $ \e -> warningM logReplyDB $ "Could not back up " ++ replyDBFile ++ ": " ++ show (e :: SomeException)
	renameFile replyDBTemporaryFile replyDBFile


replyDBFetch :: PB.Message ctx a -> IO (Maybe PB.AddrSpec)
replyDBFetch mail = do
	replyDB <- replyDBRead
	debugM logReplyDB $ "InReplyTo: " ++ show (view PB.headerInReplyTo mail) ++ ", References: " ++ show (view PB.headerReferences mail)
	pure $ do
		bs <- asum $ map (\mid -> Map.lookup (PB.renderMessageID mid) replyDB) $ L.nub $ view PB.headerInReplyTo mail ++ view PB.headerReferences mail
		either error pure $ PB.parse (mailboxToSpec <$> PB.mailbox PB.defaultCharsets) bs


replyDBAdd :: PB.Message ctx a -> IO ()
replyDBAdd mail = do
	replyDB <- replyDBRead
	fromMaybe mzero $ do
		addr    <- listToMaybe $ getFromAddrs mail
		mid     <- view PB.headerMessageID mail
		Just $ replyDBWrite $ Map.insert (PB.renderMessageID mid) (PB.renderAddressSpec addr) replyDB
