module Main where

import Data.Binary.Get
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Strings
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as MP
import MavlinkHelper
import XmlParser
import System.Environment
import System.Exit
import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Common 


runUDPServer :: IO ()
runUDPServer = do
    addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    forever (getDataFromSocket sock)
    close sock

getDataFromSocket :: Socket -> IO ()
getDataFromSocket sock = do
    print "UDP server is waiting"
    message <- recv sock 4096 
    let mavpkt = runGet decodeMavlink2Pkt message
    if (getMsgId mavpkt) == global_position_int_id
       then print $ getGlobal_position_int mavpkt
    else
       print $ getMsgId mavpkt

forever :: IO () -> IO ()
forever a = a >> forever a 

main :: IO()
main= runUDPServer

