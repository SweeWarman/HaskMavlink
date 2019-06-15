module Main where

import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Strings
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as MP
import MavlinkHelper
import System.Environment
import System.Exit
import System.IO
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send, recvFrom, sendAllTo)
import Common 


runUDPServer :: IO ()
runUDPServer = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE] } 
    addrinfos2 <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7001")
    let clientaddr = head addrinfos2
    sock <- socket (addrFamily clientaddr) Datagram defaultProtocol
    connect sock (addrAddress clientaddr)
    let hbt = Heartbeat 1 0 0 0 0 3 
    val <- send sock (BL.toStrict (get_heartbeat_bytes hbt 0 0 0)) 
    for 2 (getDataFromSocket sock)
   
getDataFromSocket :: Socket -> IO ()
getDataFromSocket sock = do
    message <- recv sock 4096 
    let mavpkt = runGet decodeMavlink2Pkt (BL.fromStrict message)
    print mavpkt
    if (getMsgId mavpkt) == global_position_int_id
       then (do 
            let gps = fromJust $ decode_global_position_int_mavpkt mavpkt
            print gps 
            send sock (BL.toStrict (get_global_position_int_bytes gps 2 0 0)) 
            return ()
            )
    else if (getMsgId mavpkt) == param_value_id
       then (do
            let pvalue = fromJust $ decode_param_value_mavpkt mavpkt 
            print pvalue
            send sock (BL.toStrict (get_param_value_bytes pvalue 3 0 0)) 
            return ()
            )
    else return ()
      
for :: Int -> IO () -> IO ()
for n a = if n > 0 then a >> for (n-1) a else return () 

main :: IO()
main= runUDPServer
