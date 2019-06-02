module Lib where

    {- 
import 

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
                             _timestamp <- getWord32le
                            ; _lat  <- getInt32le
                            ; _lon <- getInt32le
                            ; _alt <- getInt32le
                            ; _relative_alt <- getInt32le
                            ; _vx <- getInt16le
                            ; _vy <- getInt16le
                            ; _vz <-  getInt16le
                            ; _hdg <- getWord16le
                            ; return $ GlobalPositionInt _timestamp _lat _lon _alt _relative_alt _vx _vy _vz _hdg}

getGlobalPositionInt::Mavlink2Pkt -> GlobalPositionInt
getGlobalPositionInt mavpkt = runGet decodeGlobalPositionInt (BS.pack fullPayload)
                                    where
                                        truncPayload = payload mavpkt
                                        lenPayload = length truncPayload
                                        fullPayload = if lenPayload < globalPositionIntLen then 
                                                        truncPayload ++ [0 | i<-[1..(globalPositionIntLen -lenPayload)]]
                                                      else truncPayload
-}
