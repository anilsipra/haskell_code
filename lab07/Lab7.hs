{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.List (intercalate)
import qualified Network.Socket as Socket -- $ cabal install network
import           System.Environment
import           System.IO
import           Text.Regex -- $ cabal install regex-compat


defaultPortNumber :: Socket.PortNumber
defaultPortNumber = 2222

htmlFolder :: String
htmlFolder = "public_html"


data HTTPRequest =
    GetRequest { path :: String }
    deriving (Show)

data HTTPResponse =
    HTTPResponse { status :: B.ByteString, body :: B.ByteString }
    deriving (Show)

statusOk, statusBadRequest, statusNotFound :: B.ByteString
statusOk         = "200 OK"
statusBadRequest = "400 Bad Request"
statusNotFound   = "404 Not Found"

badRequestBody, notFoundBody :: B.ByteString
badRequestBody = "<!DOCTYPE html><html><head><title>Bad Request</title></head><body><h1>Bad request.</h1>It could be my fault. When I grow up I want to be a full-featured HTTP server.</body></html>"
notFoundBody   = "<!DOCTYPE html><html><head><title>Not Found</title></head><body><h1>404!</h1>File not found!</body></html>"

byteStringToString :: B.ByteString -> String
byteStringToString = map (toEnum . fromEnum) . B.unpack


-- We log strings as we perform IO over TCP with the client.
type Logger a = WriterT [String] IO a


newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }


execWriterT :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriterT writerT = do
    (_, written) <- runWriterT writerT
    return written


tell :: Applicative m => w -> WriterT w m ()
tell thingToWrite = WriterT $ pure ((),thingToWrite)


censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f writerT = WriterT $ fmap (fmap f) (runWriterT writerT) 


instance Functor m => Functor (WriterT w m) where
    fmap :: Functor m => (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f writerTArg = WriterT $ fmap (\(a,w) -> (f a,w)) (runWriterT writerTArg)


instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: (Monoid w, Applicative m) => a -> WriterT w m a
    pure a = WriterT $ pure (a,mempty)

    (<*>) :: (Monoid w, Applicative m) => WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    writerTFunc <*> writerTArg = 
        WriterT $ f <$> ((fst <$> (runWriterT writerTFunc)) <*> 
                        (fst <$> (runWriterT writerTArg))) <*>
                    ((<>) <$> ((snd <$> (runWriterT writerTFunc))) <*> 
                    (snd <$> (runWriterT writerTArg))) where
                        f a b = (a,b)


instance (Monoid w, Monad m) => Monad (WriterT w m) where
    (>>=) :: (Monoid w, Monad m) => WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    writerT >>= f = WriterT $ do
        (a,w1) <- runWriterT writerT
        runWriterT $ f a



instance Monoid w => MonadTrans (WriterT w) where
    lift :: (Monoid w, Monad m) => m a -> WriterT w m a
    lift monad = WriterT $ do
        a <- monad
        pure $ (a,mempty)


main :: IO ()
main = Socket.withSocketsDo $ do 
    args <- getArgs
    let portNumber = if null args
                     then defaultPortNumber
                     else read . head $ args
    sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption sock Socket.ReuseAddr 1
    Socket.bind sock (Socket.SockAddrInet portNumber 0)
    Socket.listen sock Socket.maxListenQueue
    putStrLn $ "Waiting for connections on port " ++ show portNumber ++ "..."
    hFlush stdout
    acceptConnectionsLoop sock


acceptConnectionsLoop :: Socket.Socket -> IO ()
acceptConnectionsLoop sock = do
    (connSocket, peerAddress) <- Socket.accept sock
    connHandle <- Socket.socketToHandle connSocket ReadWriteMode
    hSetBuffering connHandle LineBuffering
    putStrLn $ "Connection accepted from " ++ show peerAddress
    hFlush stdout
    _ <- forkIO (handleConn connHandle peerAddress)
    acceptConnectionsLoop sock


handleConn :: Handle -> Socket.SockAddr -> IO ()
handleConn connHandle peerAddress = do
    logLines <- run $ readAndRespond connHandle
    putStr $ unlines logLines
    hFlush stdout
    where
        run :: Logger () -> IO [String]
        run = execWriterT . censor prependPeerName

        prependPeerName :: [String] -> [String]
        prependPeerName logLines =
            map (prefix ++) logLines
            where
                prefix = "[" ++ show peerAddress ++ "] "


readAndRespond :: Handle -> Logger ()
readAndRespond connHandle = do
    maybeReqLines <- lift $ maybeReadRequestLines connHandle
    case maybeReqLines of
        Nothing       -> tell ["Connection broken."]
        Just reqLines ->
            case maybeParseRequestLines reqLines of
                Nothing      -> doResponse $ HTTPResponse statusBadRequest badRequestBody
                Just request -> do
                        tell ["Requested " ++ path request]
                        response <- lift $ createResponse request
                        doResponse response
    where
        doResponse response = do
            lift $ B.hPut connHandle (serializeResponse response)
            tell [byteStringToString $ status response]
            lift $ hClose connHandle
            tell ["Connection closed."]


maybeReadRequestLines :: Handle -> IO (Maybe [String])
maybeReadRequestLines connHandle = runMaybeT $ do
    line <- clean <$> MaybeT (maybeReadLine connHandle)
    if line == "" then
        return []
    else do
        rest <- MaybeT (maybeReadRequestLines connHandle)
        return $ line : rest
    where
        clean :: String -> String
        clean = filter (not . (`elem` ("\r\n" :: String)))
        maybeReadLine :: Handle -> IO (Maybe String)
        maybeReadLine h = (Just <$> hGetLine h) `catch` excepToNothing
        excepToNothing :: IOError -> IO (Maybe a)
        excepToNothing _ = return Nothing


maybeParseRequestLines :: [String] -> Maybe HTTPRequest
maybeParseRequestLines []            = Nothing
maybeParseRequestLines (firstLine:_) =
    case words firstLine of
        [ "GET", path, "HTTP/1.1" ] -> Just $ GetRequest path
        _                           -> Nothing


createResponse :: HTTPRequest -> IO HTTPResponse
createResponse (GetRequest path) = do
    let filePaths = requestPathToFilePaths path
    maybeContents <- maybeReadOneFile filePaths
    return $
        case maybeContents of
            Just fileContents -> HTTPResponse statusOk fileContents
            Nothing           -> HTTPResponse statusNotFound notFoundBody


maybeReadOneFile :: [String] -> IO (Maybe B.ByteString)
maybeReadOneFile []          = return Nothing
maybeReadOneFile (path:rest) =
    (Just <$> B.readFile path) `catch` tryNext
    where
        tryNext :: IOError -> IO (Maybe B.ByteString)
        tryNext _ = maybeReadOneFile rest

requestPathToFilePaths :: String -> [String]
requestPathToFilePaths path =
      let parts = htmlFolder : splitRegex (mkRegex "/|\\\\|\\.\\.") path in
      [ intercalate "/"  parts
      , intercalate "/"  (parts ++ ["index.html"])
      , intercalate "\\" parts
      , intercalate "\\" (parts ++ ["index.html"])
      ]


serializeResponse :: HTTPResponse -> B.ByteString
serializeResponse (HTTPResponse statusCode body) =
    B.concat
        [ "HTTP/1.1 ", statusCode, crlf
        , "Content-Type: text/html; charset=UTF-8", crlf
        , "Content-Length: ", lengthStr, crlf
        , "Server: Lab 7 Silly Haskell Server 1.0", crlf
        , "Connection: close", crlf
        , "", crlf
        , body
        ]
    where
        crlf      = "\r\n" 
        lengthStr = C.pack . show . B.length $ body

