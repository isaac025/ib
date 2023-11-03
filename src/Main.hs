module Main where

import Network.Socket qualified as N
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
main = do
    h <- connectTo myServer myPort
    t <- hGetContents h
    hSetBuffering stdout NoBuffering
    print t

-- connect to server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    (addr : _) <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode
