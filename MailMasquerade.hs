{-# LANGUAGE FlexibleInstances, StandaloneDeriving, Strict, TypeOperators, DataKinds, OverloadedStrings, DeriveGeneric #-}
module MailMasquerade where

import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, toEncoding)
import qualified Data.Aeson as JSON
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Function
import qualified Data.IMF as PB
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.MIME as PB
import Data.String
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL (connectIMAPSSL)
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.Internal
import Network.HaskellNet.SMTP.SSL (doSMTPSSL)
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Options.Generic
import System.Directory
import System.Log.Logger
import System.Log.Handler.Syslog
import Text.Parsec (parse)
import Text.Parsec.Rfc2822

-- a map from Message-IDs to sender addresses
-- populated by every message coming from the outside world
type ReplyDB = Map Text Text

data Arguments f = Arguments
	{ verbose :: f ::: Bool   <#> "v" <?> "More logs"
	, stdout  :: f ::: Bool   <#> "s" <?> "Put log to stdout only"
	, config  :: f ::: String <#> "c" <?> "Path to config file"
	} deriving Generic

instance ParseRecord (Arguments Wrapped)
deriving instance Show (Arguments Unwrapped)

data Config = Config
	{ username :: String
	, password :: String
	, target :: String
	, whitelist :: [Text]
	, defaultReplyTo :: [Text]	-- addresses to send the mail to when we're unable to figure who the target is replying to
	} deriving (Show, Generic)
instance FromJSON Config where
instance ToJSON Config where
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
	conf <- either error id <$> JSON.eitherDecodeFileStrict (config args)
	infoM "" $ "Opened configuration file " ++ config args
	fetchMail conf

nameAddrToAddress :: NameAddr -> Address
nameAddrToAddress (NameAddr name addr) = Address (fmap fromString name) $ fromString addr

-- our header parser doesn't decode utf-8 addresses, but our serializer does encode them
nameAddrToText :: NameAddr -> Text
nameAddrToText (NameAddr name addr) = T.pack $ (maybe "" (\n -> n ++ " ") name) ++ "<" ++ addr ++ ">"

textToAddress :: Text -> Address
textToAddress str = nameAddrToAddress $ fromRight (error "couldn't parse a stored address unexpectedly") $ parse mailbox "<address>" str

purgeReplyTo :: [PB.Header] -> [PB.Header]
purgeReplyTo = L.deleteBy (on (==) fst) ("reply-to", undefined)

adjustMailReply (PB.Message (PB.Headers headers) body) from to = PB.renderMessage $ PB.Message (PB.Headers $ map
	(\(k, v) -> case () of _
				| k == "from" -> (k, fromString from)
				| k == "to" -> (k, S.fromString $ S.toString to)
				| otherwise -> (k, v)
	) $ purgeReplyTo headers) body

adjustMailForForwarding :: (PB.RenderMessage a) => PB.Message s2 a -> String -> String -> BL.ByteString
adjustMailForForwarding (PB.Message (PB.Headers headers) body) from to = PB.renderMessage $ PB.Message (PB.Headers $
	("From", fromString from) : ("To", S.fromString $ S.toString to) : map
	(\(k, v) -> case () of _
				| k == "from" -> ("X-Original-From", v)
				| k == "to" -> ("X-Original-To", v)
				| otherwise -> (k, v)
	) headers) body

parseMessage :: ByteString -> Maybe [Field]
parseMessage mail = either (const Nothing) (\(Message fields _) -> Just fields) $ parse message "<inbound>" mail

getFrom :: [Field] -> Maybe Text
getFrom = listToMaybe . mapMaybe (\x -> case x of
		From [addr] -> Just $ nameAddrToText addr
		_ -> Nothing
	)

getFromAddr :: [Field] -> Maybe ByteString
getFromAddr = listToMaybe . mapMaybe (\x -> case x of
		From [addr] -> Just $ fromString $ nameAddr_addr addr
		_ -> Nothing
	)

getInReplyTo :: [Field] -> Maybe Text
getInReplyTo = listToMaybe . mapMaybe (\x -> case x of
		InReplyTo [str] -> Just $ fromString str
		_ -> Nothing
	)

getMessageID :: [Field] -> Maybe Text
getMessageID = listToMaybe . mapMaybe (\x -> case x of
		MessageID str -> Just $ fromString str
		_ -> Nothing
	)

isInternalMail :: Config -> ByteString -> Bool
isInternalMail conf mail = let Right parsedMessage = parse message "<inbound>" mail in
	let (Message headers _) = parsedMessage in
	let NameAddr _ from = head $ mapMaybe (\x -> case x of
			From x -> Just $ head x
			_ -> Nothing
		) headers in
		from == (target conf)

tossMail conf mail to = doSMTPSSL "smtp.yandex.ru" $ \conn -> do
	authSuccess <- SMTP.authenticate PLAIN (username conf) (password conf) conn
	if not authSuccess then error "authentication failed" else do
		sendMailData (Address Nothing $ T.pack $ username conf) [to] (BL.toStrict mail) conn

fetchMail conf = do
	forever $ handle (\e -> errorM "" $ show (e :: IOException)) $ do
		conn <- connectIMAPSSL "imap.yandex.ru"
		login conn (username conf) (password conf)
		forever $ do
			grabNewMail conf conn
			idle conn $ 1000 * 60 * 29	-- rfc9051
			putStrLn "got new mail maybe"

grabNewMail conf conn = do
	select conn "INBOX"
	msgs <- search conn [UNFLAG Seen]
	--putStrLn $ "Unseen message IDs: " ++ show msgs
	forM_ msgs (fetch conn >=> handleNewMail conf)

-- \n-delimited addresses
-- doesn't handle \r
whitelistFile = "whitelist.txt"

checkIfWhitelisted :: ByteString -> IO Bool
checkIfWhitelisted addr = do
	whitelistContents <- B.readFile whitelistFile
	pure $ elem addr $ B.lines whitelistContents

handleNewMail :: Config -> ByteString -> IO ()
handleNewMail conf mail = do
	case PB.parse (PB.message PB.mime) mail of
		Left e -> errorM "" $ show e
		Right parsedMail -> do
			if isInternalMail conf mail
			then do
				addr <- replyDBFetch mail
				let sendTo = maybe (defaultReplyTo conf) pure addr
				mapM_ (\addr_ -> tossMail conf (adjustMailReply parsedMail (username conf) addr_) $ textToAddress addr_) sendTo
			else do
				whitelisted <- checkIfWhitelisted $ fromJust $ getFromAddr $ fromJust $ parseMessage mail
				when whitelisted $ do
					tossMail conf (adjustMailForForwarding parsedMail (username conf) (target conf)) $ Address Nothing $ T.pack $ target conf
					replyDBAdd mail
				pure ()

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

replyDBFetch :: ByteString -> IO (Maybe Text)
replyDBFetch mail = do
	replyDB <- replyDBRead
	pure $ do
		headers <- parseMessage mail
		mid <- getInReplyTo headers
		Map.lookup mid replyDB

replyDBAdd :: ByteString -> IO ()
replyDBAdd mail = do
	replyDB <- replyDBRead
	fromMaybe mzero $ do
		headers <- parseMessage mail
		addr <- getFrom headers
		mid <- getMessageID headers
		Just $ replyDBWrite $ Map.insert mid addr replyDB
