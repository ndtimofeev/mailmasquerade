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
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL (connectIMAPSSL)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.SMTP.Internal
import Network.HaskellNet.SMTP.SSL (doSMTPSSL)
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Options.Generic
import System.Directory
import System.Log.Logger
import System.Log.Handler.Syslog

-- a map from Message-IDs to sender addresses
-- populated by every message coming from the outside world
type ReplyDB = Map ByteString ByteString


data Arguments f = Arguments
	{ verbose :: f ::: Bool   <#> "v" <?> "More logs"
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


main :: IO ()
main = do
	args <- unwrapRecord "MailMasquerade"
	when (verbose args) $ do
		updateGlobalLogger rootLoggerName (setLevel INFO)
		infoM "" $ "Set verbose mode"
	when (not (stdout args)) $ do
		s <- openlog "mailmasquerade" [PID] USER INFO
		updateGlobalLogger rootLoggerName (addHandler s)
	bs   <- B.readFile (config args)
	let conf' = either error id $ JSON.eitherDecodeStrict bs >>= traverse (PB.parse (mailboxToSpec <$> PB.mailbox PB.defaultCharsets) . Data.ByteString.Lazy.Char8.pack)
	-- conf <- either error id <$> JSON.eitherDecodeFileStrict (config args)
	infoM "" $ "Opened configuration file " ++ config args
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


tossMail :: Config PB.AddrSpec -> BL.ByteString -> Address -> IO ()
tossMail conf mail to = doSMTPSSL (smtpServer conf) $ \conn -> do
	authSuccess <- SMTP.authenticate PLAIN (specToString $ username conf) (password conf) conn
	when (not authSuccess) $ error "authentication failed"
	sendMailData (specToAddress $ username conf) [to] (BL.toStrict mail) conn


fetchMail :: Config PB.AddrSpec -> IO ()
fetchMail conf = do
	forever $ handle (\e -> errorM "" $ show (e :: IOException)) $ do
		conn <- connectIMAPSSL (imapServer conf)
		login conn (specToString $ username conf) (password conf)
		forever $ do
			grabNewMail conf conn
			idle conn $ 1000 * 60 * 29	-- rfc9051
			infoM "" $ "got new mail maybe"


grabNewMail :: Config PB.AddrSpec -> IMAPConnection -> IO ()
grabNewMail conf conn = do
	select conn "INBOX"
	msgs <- search conn [UNFLAG Seen]
	infoM "" $ "Unseen message IDs: " ++ show msgs
	forM_ msgs (fetch conn >=> handleNewMail conf)


addrToSpec :: PB.Address -> [PB.AddrSpec]
addrToSpec addr = case addr of
	PB.Single mbox   -> [mailboxToSpec mbox]
	PB.Group _ mboxs -> map mailboxToSpec mboxs


mailboxToSpec :: PB.Mailbox -> PB.AddrSpec
mailboxToSpec (PB.Mailbox _ spec) = spec


handleNewMail :: Config PB.AddrSpec -> ByteString -> IO ()
handleNewMail conf mail = do
	infoM "" $ "Lets handle new mail!"
	case PB.parse (PB.message PB.mime) mail of
		Left e -> errorM "" $ show e
		Right parsedMail@(PB.Message (PB.Headers hdrs) _) -> do
			infoM "" $ unlines $ map show hdrs
			let fromAddrs  = getFromAddrs parsedMail
			if	| target conf `elem` fromAddrs -> do
					infoM "" $ "This is remote mail"
					maddr <- replyDBFetch parsedMail
					let sendTo = maybe (defaultReplyTo conf) pure maddr
					infoM "" $ "Let send mail to " ++ show sendTo
					forM_ sendTo $ \addr -> do
						let newMail@(PB.Message (PB.Headers hdrs) _) = adjustMailReply parsedMail (username conf) addr
						infoM "" $ unlines $ map show hdrs
						tossMail conf (PB.renderMessage newMail) $ specToAddress addr

				| Just spec <- listToMaybe fromAddrs
				, spec `elem` whitelist conf -> do
					infoM "" $ "This is local mail"
					infoM "" $ "Let send mail to " ++ specToString (target conf)
					tossMail conf (PB.renderMessage $ adjustMailForForwarding parsedMail (username conf) (target conf)) $ specToAddress $ target conf
					replyDBAdd parsedMail

				| otherwise -> pure ()


replyDBFile = "replydb.bin"
replyDBTemporaryFile = "replydb.bin.tmp"
replyDBBackupFile = "replydb.bin.bak"


-- creates an empty database on read failure
replyDBRead :: IO ReplyDB
replyDBRead = catch (do
		replyDBBinary <- BL.readFile replyDBFile
		pure $ decode replyDBBinary
	) $ \e -> do
		errorM "" $ show (e :: IOException)
		pure mempty


replyDBWrite :: ReplyDB -> IO ()
replyDBWrite replyDB = do
	BL.writeFile replyDBTemporaryFile $ encode replyDB
	catch (renameFile replyDBFile replyDBBackupFile) $ \e -> errorM "" $ show (e :: IOException)
	renameFile replyDBTemporaryFile replyDBFile


replyDBFetch :: PB.Message ctx a -> IO (Maybe PB.AddrSpec)
replyDBFetch mail = do
	replyDB <- replyDBRead
	infoM "" $ "InReplyTo: " ++ show (view PB.headerInReplyTo mail)
	infoM "" $ "References: " ++ show (view PB.headerReferences mail)
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
