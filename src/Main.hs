module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bool (bool)
import Data.List
import Data.Time
import Network.Socket qualified as N
import System.Exit
import System.IO

-- configuration options
myServer :: String
myServer = "irc.freenode.org"

myPort :: N.PortNumber
myPort = 6667

myChan :: String
myChan = "#tutbot-testing"

myNick :: String
myNick = "tutbot"

-- top-level program
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop = runReaderT run

-- The Net Monad, a wrapper over IO carrying the bot's immutable state
data Bot = Bot
    { botSocket :: Handle
    , startTime :: UTCTime
    }
type Net = ReaderT Bot IO

-- connnect to server and return the initial bot state
connect :: IO Bot
connect = notifiy $ do
    t <- getCurrentTime
    h <- connectTo myServer myPort
    pure (Bot h t)
  where
    notifiy =
        bracket_
            (putStrLn ("Connecting to " ++ myServer ++ " ...") >> hFlush stdout)
            (putStrLn "done.")

-- connect to server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    (addr : _) <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- join a channel and start processing commands
run :: Net ()
run = do
    write "NICK" myNick
    write "USER" (myNick ++ " 0 * :tutorial bot")
    write "JOIN" myChan
    listen

-- send a message to a handle
write :: String -> String -> Net ()
write cmd args = do
    h <- asks botSocket
    let msg = cmd ++ " " ++ args ++ "\r\n"
    liftIO $ hPutStr h msg -- send msg on the wire
    liftIO $ putStr ("> " ++ msg) -- show sent msg ont the cli

-- process each line from the server
listen :: Net ()
listen = forever $ do
    h <- asks botSocket
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    if isPing s then pong s else eval (clean s)
  where
    forever :: Net () -> Net ()
    forever a = do a; forever a
    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1
    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x
    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)

-- dispatch command
eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval "!uptime" = uptime >>= privmsg
eval x | "!id" `isPrefixOf` x = privmsg (drop 4 x)
eval _ = pure ()

-- get the current uptime
uptime :: Net String
uptime = do
    now <- liftIO getCurrentTime
    zero <- asks startTime
    pure (pretty (diffUTCTime now zero))

-- send a privmsg to the chan
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (myChan ++ " :" ++ s)

-- pretty print the date in '1d 9h 9m 17s' format
pretty :: NominalDiffTime -> String
pretty diff = unwords . map (\(t, unit) -> show t ++ unit) $ bool [(0, "s")] diffs (null diffs)
  where
    diffs :: [(Integer, String)]
    diffs = filter ((/= 0) . fst) $ decompose [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")] (floor diff)
    decompose [] _ = []
    decompose ((secs, unit) : metrics) t =
        let (n, t') = t `divMod` secs
         in (n, unit) : decompose metrics t'
