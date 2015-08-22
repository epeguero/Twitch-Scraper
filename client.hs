
import Network.Socket
import System.IO

import Config
import IrcParser

main :: IO ()
main = do
        sock <- createConnection serverAddr serverPort
        hdl <- convertSocket sock
        sendLogin hdl botName botOAuth
        mainLoop hdl

mainLoop :: Handle -> IO ()
mainLoop hdl = do
        msg <- hGetLine hdl
        processCommand hdl (parseMessage msg)
        mainLoop hdl

createConnection :: String -> String -> IO Socket 
createConnection server port = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1 
        let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV, AI_CANONNAME] }
        addrs <- getAddrInfo (Just hints) (Just server) (Just port)
        let addr = head addrs
        connect sock (addrAddress addr)
        return sock

convertSocket :: Socket -> IO Handle 
convertSocket sock = do
        hdl <- socketToHandle sock ReadWriteMode
        hSetBuffering hdl NoBuffering
        return hdl

sendCommand :: Handle -> String -> IO ()
sendCommand hdl cmd = hPutStrLn hdl cmd

sendLogin :: Handle -> String -> String -> IO ()
sendLogin hdl nick pass = do
        sendCommand hdl ("PASS " ++ pass)
        sendCommand hdl ("NICK " ++ nick)

sendJoin :: Handle -> String -> IO ()
sendJoin hdl chn = sendCommand hdl ("JOIN " ++ chn)

processCommand :: Handle -> IrcMessage -> IO () 
processCommand hdl msg
        | cmd == "376" = onLogin hdl
        | cmd == "PING" = sendCommand hdl ("PONG " ++ (head (args msg)))
        | cmd == "PRIVMSG" = processPrivMsg hdl msg
        | otherwise = putStrLn . show $ msg
        where
                cmd = command msg

processPrivMsg :: Handle -> IrcMessage -> IO()
processPrivMsg hdl msg = putStrLn (username ++ " says: " ++ text)
        where 
                username = takeWhile (/= '!') (prefix msg)
                text = tail (last (args msg))

onLogin :: Handle -> IO ()
onLogin hdl = do 
        sendCommand hdl "CAP REQ :twitch.tv/membership twitch.tv/commands twitch.tv/tags"
        sendJoin hdl "#lirik"
