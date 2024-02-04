{-# LANGUAGE Strict, OverloadedStrings, DeriveGeneric #-}
module Main where

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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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
import Network.Mail.Mime
import System.Directory
import System.Log.Logger
import System.Log.Handler.Syslog
import Text.Parsec (parse)
import Text.Parsec.Rfc2822
import Text.RE.TDFA.ByteString

-- a map from Message-IDs to sender addresses
-- populated by every message coming from the outside world
type ReplyDB = Map Text Text

data Config = Config
	{ username :: String
	, password :: String
	, target :: String
	, defaultReplyTo :: [Text]	-- addresses to send the mail to when we're unable to figure who the target is replying to
	} deriving (Show, Generic)
instance FromJSON Config where
instance ToJSON Config where
	toEncoding = JSON.genericToEncoding JSON.defaultOptions

configFile = "mailmasquerade.json"

readConfig :: IO Config
readConfig = fmap (either error id) $ JSON.eitherDecodeFileStrict configFile

main :: IO ()
main = do
	s <- openlog "mailmasquerade" [PID] USER INFO
	updateGlobalLogger rootLoggerName (addHandler s)
	conf <- readConfig
	infoM "" $ "Opened configuration file " ++ configFile
	fetchMail conf

nameAddrToAddress :: NameAddr -> Address
nameAddrToAddress (NameAddr name addr) = Address (fmap fromString name) $ fromString addr

-- our header parser doesn't decode utf-8 addresses, but our serializer does encode them
nameAddrToText :: NameAddr -> Text
nameAddrToText (NameAddr name addr) = T.pack $ (maybe "" (\n -> n ++ " ") name) ++ "<" ++ addr ++ ">"

textToAddress :: Text -> Address
textToAddress str = nameAddrToAddress $ fromRight (error "couldn't parse a stored address unexpectedly") $ parse mailbox "<address>" str

-- there are no compatible e-mail parsers and serializers, apparently, so regex seems like an optimal option for header modification
adjustMailForForwarding mail from to = mail
	?=~/ (fromJust $ compileSearchReplaceWith MultilineInsensitive "^from: (.*)$" ("X-Original-From: $1\nFrom: " ++ (S.fromString $ S.toString from)))
	?=~/ (fromJust $ compileSearchReplaceWith MultilineInsensitive "^to: (.*)$" ("X-Original-To: $1\nTo: " ++ (S.fromString $ S.toString to)))

adjustMailReply :: ByteString -> String -> Text -> ByteString
adjustMailReply mail from to = mail
	?=~/ (fromJust $ compileSearchReplaceWith MultilineInsensitive "^from: (.*)$" ("From: " ++ (fromString from)))
	?=~/ (fromJust $ compileSearchReplaceWith MultilineInsensitive "^to: (.*)$" ("To: " ++ (S.fromString $ S.toString to)))

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
		sendMailData (Address Nothing $ T.pack $ username conf) [to] mail conn

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
	if isInternalMail conf mail
	then do
		addr <- replyDBFetch mail
		let sendTo = maybe (defaultReplyTo conf) pure addr
		mapM_ (\addr_ -> tossMail conf (adjustMailReply mail (username conf) addr_) $ textToAddress addr_) sendTo
	else do
		whitelisted <- checkIfWhitelisted $ fromJust $ getFromAddr $ fromJust $ parseMessage mail
		when whitelisted $ do
			tossMail conf (adjustMailForForwarding mail (username conf) (target conf)) $ Address Nothing $ T.pack $ target conf
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
