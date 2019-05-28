module Lib
    ( 
      Mavlink2Pkt,
      GlobalPositionInt,
      decodeGlobalPositionInt,
      decodeMavlink2Pkt,
      mavlinkPkt2word8,
      getGlobalPositionInt,
      gen_crc25
    ) where

import           Data.Binary.Get
import           Data.Word
import           Data.Bits
import           Data.Int
import qualified Data.ByteString.Lazy as BS

-- Type Aliases for fixes size unsigned integers
type Uint8  = Word8
type Uint16 = Word16
type Uint32 = Word32
type Uint64 = Word64



-- Helper functions to serial fixed size integers
cnvW16toW8 :: Uint16 -> [Uint8]
cnvW16toW8 input = [fromIntegral(input), fromIntegral(input `shiftR` 8)]


cnvW32toW8 :: Uint32 -> [Uint8]
cnvW32toW8 input = (cnvW16toW8 $ fromIntegral input) ++
                   (cnvW16toW8 $ fromIntegral (input `shiftR` 16))


cnvW64toW8 :: Uint64 -> [Uint8]
cnvW64toW8 input = (cnvW32toW8 $ fromIntegral input) ++
                   (cnvW32toW8 $ fromIntegral (input `shiftR` 32))


-- function to generate the checksum accumulator
crc25_acc :: Word16 -> Word8 -> Word16
crc25_acc  accum byte = (accum `shiftR` 8) `xor` val1 `xor` val2 `xor` val3
                            where
                                acc  = (accum .&. 0xff)
                                tmp' = byte `xor` (fromIntegral acc)
                                tmp  = (tmp' `xor` (tmp' `shiftL` 4)) .&. 0xff
                                val1 = (fromIntegral tmp) `shiftL` 8
                                val2 = (fromIntegral tmp) `shiftL` 3
                                val3 = (fromIntegral tmp) `shiftR` 4

-- Compute CRC on given data
gen_crc25:: [Word8] -> Word16
gen_crc25 inputdata = foldl crc25_acc 0xffff inputdata

-- Definition of a mavlink packet
data Mavlink2Pkt = Mavlink2Pkt {
                      magic          :: Uint8
                    , len            :: Uint8
                    , incompat_flags :: Uint8
                    , compat_flags   :: Uint8
                    , seqm           :: Uint8
                    , sysid          :: Uint8
                    , compid         :: Uint8
                    , msgid          :: [Uint8] 
                    , payload        :: [Uint8] 
                    , checksum       :: Uint16 
                    , signature      :: [Uint8]
}deriving (Show)


truncPayload payload = if (last payload) == 0
                              then truncPayload (init payload)
                       else payload
                         
-- Function to serialize the mavlink packet
mavlinkPkt2word8 :: Mavlink2Pkt -> Uint8 -> [Uint8]
mavlinkPkt2word8 mavpkt crcextra = [trunclen] ++ ([incompat_flags,compat_flags,seqm,sysid,compid] <*> [mavpkt]) ++ 
                          (msgid mavpkt) ++ _payload ++ [crcextra]
                              where _payload = truncPayload (payload mavpkt) 
                                    trunclen = fromIntegral (length _payload)

-- Function to decode a mavlink packet from a lazy byte stream
decodeMavlink2Pkt :: Get Mavlink2Pkt
decodeMavlink2Pkt = do {
                        _magic <- getWord8
                       ;_len   <- getWord8
                       ;_incompat_flags <- getWord8
                       ;_compat_flags <- getWord8
                       ;_seq <- getWord8
                       ;_sysid <- getWord8
                       ;_compid <- getWord8
                       ;_msgid  <- mapM (\x->getWord8) [1,2,3]
                       ;_payload <- mapM (\x->getWord8) [i | i <- [1.._len]]
                       ;_checksum <- getWord16le
                       ;let mvlksigned = (_incompat_flags .&. 0x01) == 1
                       ;_signature <- if mvlksigned then mapM (\x->getWord8) [i | i <- [1..13]] else (return [0])
                       ;return $ Mavlink2Pkt _magic _len _incompat_flags _compat_flags 
                                             _seq _sysid _compid _msgid _payload _checksum _signature
                      }

-- Message specific data types
data GlobalPositionInt = GlobalPositionInt {
                      time_boot_ms :: Uint32
                    , lat          :: Int32
                    , lon          :: Int32
                    , alt          :: Int32
                    , relative_alt :: Int32
                    , vx           :: Int16
                    , vy           :: Int16
                    , vz           :: Int16
                    , hdg          :: Uint16
}deriving (Show)

globalPositionIntLen = 28 


decodeGlobalPositionInt :: Get GlobalPositionInt
decodeGlobalPositionInt = do {
                             _timestamp <- fromIntegral <$> getWord32le
                            ; _lat  <- fromIntegral <$> getInt32le
                            ; _lon <- fromIntegral <$> getInt32le
                            ; _alt <- fromIntegral <$> getInt32le
                            ; _relative_alt <- fromIntegral <$> getInt32le
                            ; _vx <- fromIntegral <$> getInt16le
                            ; _vy <- fromIntegral <$> getInt16le
                            ; _vz <- fromIntegral <$> getInt16le
                            ; _hdg <- fromIntegral <$> getWord16le
                            ; return $ GlobalPositionInt _timestamp _lat _lon _alt _relative_alt _vx _vy _vz _hdg}

getGlobalPositionInt::Mavlink2Pkt -> GlobalPositionInt
getGlobalPositionInt mavpkt = runGet decodeGlobalPositionInt (BS.pack fullPayload)
                                    where
                                        truncPayload = payload mavpkt
                                        lenPayload = length truncPayload
                                        fullPayload = if lenPayload < globalPositionIntLen then 
                                                        truncPayload ++ [0 | i<-[1..(globalPositionIntLen -lenPayload)]]
                                                      else truncPayload

