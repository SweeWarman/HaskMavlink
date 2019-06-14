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
import Network.Socket hiding (recv, recvFrom, send)
import Network.Socket.ByteString.Lazy (recv, send)
import Common 


runUDPServer :: IO ()
runUDPServer = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE] } 
    addrinfos <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    forever (getDataFromSocket sock)
   
getDataFromSocket :: Socket -> IO ()
getDataFromSocket sock  = do
    print "UDP server is waiting"
    message <- recv sock 4096 
    let mavpkt = runGet decodeMavlink2Pkt message
    print mavpkt
    if (getMsgId mavpkt) == global_position_int_id
       then (do 
             let gps = fromJust $ decode_global_position_int_mavpkt mavpkt
             print gps 
             print $ BL.unpack (runPut (get_global_position_int_payload gps))
             print $ encode_global_position_int_mavpkt gps 6 255 0
             send sock (get_global_position_int_bytes gps 6 255 0)
             return ()
            )
    else
       print $ getMsgId mavpkt
      
forever :: IO () -> IO ()
forever a = a >> forever a 

main :: IO()
main= runUDPServer


    {-
getGPSPayload :: Global_position_int -> Put 
getGPSPayload gps = do
                     putWord32le (getGlobal_position_int_time_boot_ms gps) 
                     putInt32le (getGlobal_position_int_lat gps)
                     putInt32le (getGlobal_position_int_lon gps)
                     putInt32le (getGlobal_position_int_alt gps)
                     putInt32le (getGlobal_position_int_relative_alt gps) 
                     putInt16le (getGlobal_position_int_vx gps) 
                     putInt16le (getGlobal_position_int_vy gps)
                     putInt16le (getGlobal_position_int_vz gps)
                     putWord16le (getGlobal_position_int_hdg gps)

getMavpktfromGPS :: Global_position_int -> Uint8 -> Uint8 -> Uint8 -> Mavlink2Pkt
getMavpktfromGPS gps _seqm _sysid _compid=Mavlink2Pkt _magic _len _incompat_flags _compat_flags _seqm _sysid _compid _msgid _payload _chksum [0]
                   where
                       _magic = 253
                       _len = fromIntegral global_position_int_len
                       _incompat_flags = 0
                       _compat_flags = 0
                       _msgid = init $ cnvW32toW8 (fromIntegral global_position_int_id)
                       _payload = BL.unpack (runPut (getGPSPayload gps))
                       _mavpkt = Mavlink2Pkt _magic _len _incompat_flags _compat_flags _seqm _sysid _compid _msgid _payload 0 [0]
                       _chksum = gen_crc25 (mavlinkPkt2word8 _mavpkt global_position_int_crc_extra)

getBytesGlobalPositionInt::Global_position_int -> Uint8 -> Uint8 -> Uint8 -> Put
getBytesGlobalPositionInt gps _seq _sysid _compid = do
                                                   let mavpkt = getMavpktfromGPS gps _seq _sysid _compid 
                                                   putWord8 (fromIntegral 253)
                                                   putWord8 (fromIntegral global_position_int_len)
                                                   putWord8 0
                                                   putWord8 0
                                                   sequence (fmap putWord8 (msgid mavpkt))
                                                   getGPSPayload gps
                                                   putWord16le (checksum mavpkt)
                                                   putLazyByteString (BL.pack (signature mavpkt))

getBytesGPS::Global_position_int -> BL.ByteString
getBytesGPS gps = runPut (getBytesGlobalPositionInt gps 1 0 0)
-}
