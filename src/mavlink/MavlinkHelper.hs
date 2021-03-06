module MavlinkHelper where

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


getMsgId::Mavlink2Pkt -> Int
getMsgId mavpkt = fromIntegral $ (_msgid !! 0) .|. ((_msgid !! 1) `shiftL` 8) .|. ((_msgid !! 2) `shiftL` 16)
                 where 
                     _msgid = msgid mavpkt


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
getBytesForChecksum :: Mavlink2Pkt -> Uint8 -> [Uint8]
getBytesForChecksum mavpkt crcextra = [trunclen] ++ ([incompat_flags,compat_flags,seqm,sysid,compid] <*> [mavpkt]) ++ 
                          (msgid mavpkt) ++ _payload ++ [crcextra]
                              where _payload = (payload mavpkt) 
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
                       ;_payload <- mapM (\x->getWord8) [1.._len]
                       ;_checksum <- getWord16le
                       ;let mvlksigned = (_incompat_flags .&. 0x01) == 1
                       ;_signature <- if mvlksigned then mapM (\x->getWord8) [1..13] else (return [0])
                       ;return $ Mavlink2Pkt _magic _len _incompat_flags _compat_flags 
                                             _seq _sysid _compid _msgid _payload _checksum _signature
                      }

