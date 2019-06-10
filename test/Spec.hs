module Main where

import Data.Binary.Get
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
import Network.Socket hiding (recv, recvFrom, send)
import Network.Socket.ByteString.Lazy (recv, send)
import Common 


runUDPServer :: IO ()
runUDPServer = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE] } 
    addrinfos <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

    addrinfos2 <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7001")
    let serveraddr2 = head addrinfos2
    sock2 <- socket (addrFamily serveraddr2) Datagram defaultProtocol

    let hbt = Heartbeat 1 0 0 0 0 0

    connect sock2 (addrAddress serveraddr2)
    -- send sock2 $ (C.pack "Hello")
    -- message <- recv sock2 4096
    -- print message
    -- return ()


    -- bind sock (addrAddress serveraddr)
    -- forever (getDataFromSocket sock2)
    --
   
getDataFromSocket :: Socket -> IO ()
getDataFromSocket sock  = do
    print "UDP server is waiting"
    message <- recv sock 4096 
    let mavpkt = runGet decodeMavlink2Pkt message
    print mavpkt
    if (getMsgId mavpkt) == global_position_int_id
       then (do 
             let gps = fromJust $ getGlobal_position_int mavpkt
             print gps 
             print $ getGPSPayload gps
             print $ getMavpktfromGPS gps 6 255 0
             send sock (getBytesGlobalPositionInt gps)
             return ()
            )
    else
       print $ getMsgId mavpkt
      
forever :: IO () -> IO ()
forever a = a >> forever a 

main :: IO()
main= runUDPServer


getGPSPayload :: Global_position_int -> [Uint8]
getGPSPayload gps = cnvW32toW8 (getGlobal_position_int_time_boot_ms gps) ++ 
                    cnvW32toW8 (fromIntegral (getGlobal_position_int_lat gps)) ++
                    cnvW32toW8 (fromIntegral (getGlobal_position_int_lon gps)) ++
                          cnvW32toW8 (fromIntegral (getGlobal_position_int_alt gps)) ++
                          cnvW32toW8 (fromIntegral (getGlobal_position_int_relative_alt gps)) ++ 
                          cnvW16toW8 (fromIntegral (getGlobal_position_int_vx gps)) ++ 
                          cnvW16toW8 (fromIntegral (getGlobal_position_int_vy gps)) ++
                          cnvW16toW8 (fromIntegral (getGlobal_position_int_vz gps)) ++
                          cnvW16toW8 (getGlobal_position_int_hdg gps)

getMavpktfromGPS :: Global_position_int -> Uint8 -> Uint8 -> Uint8 -> Mavlink2Pkt
getMavpktfromGPS gps _seqm _sysid _compid=Mavlink2Pkt _magic _len _incompat_flags _compat_flags _seqm _sysid _compid _msgid _payload _chksum [0]
                   where
                       _magic = 253
                       _len = fromIntegral global_position_int_len
                       _incompat_flags = 0
                       _compat_flags = 0
                       _msgid = init $ cnvW32toW8 (fromIntegral global_position_int_id)
                       _payload = getGPSPayload gps
                       _mavpkt = Mavlink2Pkt _magic _len _incompat_flags _compat_flags _seqm _sysid _compid _msgid _payload 0 [0]
                       _chksum = gen_crc25 (mavlinkPkt2word8 _mavpkt global_position_int_crc_extra)

getBytesGlobalPositionInt::Global_position_int -> BL.ByteString
getBytesGlobalPositionInt gps = BL.pack (mavlinkPkt2word8 (getMavpktfromGPS gps 6 255 0) global_position_int_crc_extra)
