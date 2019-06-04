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
import Lib


runUDPServer :: IO ()
runUDPServer = do
    addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    print "UDP server is waiting"
    message <- recv sock 4096 
    let mavpkt = runGet decodeMavlink2Pkt message
    if (getMsgId mavpkt) == 33
       then print $ getGlobalPositionInt mavpkt
    else
       print $ getMsgId mavpkt
    close sock


main :: IO()
main = runUDPServer

main2 :: IO ()
main2 = do{
            bindata1 <- BL.readFile "src/test2.txt"
            ;bindata2 <- BL.readFile "src/test3.txt"
            ;let mavpkt = runGet decodeMavlink2Pkt bindata1
            -- ;let gpsmsg = getGlobalPositionInt mavpkt
            -- ;let gpsmsg2 = runGet decodeGlobalPositionInt bindata2
            -- ;let mavpktword8 =(mavlinkPkt2word8 mavpkt 104) 
            ;print $ BL.unpack bindata1
            ;print $ mavpkt
            -- ;print $ gpsmsg 
            -- ;print $ gpsmsg2
            -- ;print $ mavpktword8
            -- ;print $ gen_crc25 mavpktword8
         }

main3 :: IO ()
main3= do
    args <- getArgs
    case args of
        [filename] -> processXmlFile filename
        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

