module Main where

import Data.Binary.Get
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Strings
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as MP
import Lib
import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Proc
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC

type MavDataDict a = MP.Map String a 
type MavDictEnum = MP.Map String [String]
type MavDictDesc = MP.Map String [String]

main :: IO ()
main = do{
            bindata1 <- BL.readFile "src/test2.txt"
            ;bindata2 <- BL.readFile "src/test3.txt"
            ;let mavpkt = runGet decodeMavlink2Pkt bindata1
            ;let gpsmsg = getGlobalPositionInt mavpkt
            ;let gpsmsg2 = runGet decodeGlobalPositionInt bindata2
            ;let mavpktword8 =(mavlinkPkt2word8 mavpkt 104) 
            ;print $ mavpkt
            ;print $ gpsmsg 
            ;print $ gpsmsg2
            ;print $ mavpktword8
            ;print $ gen_crc25 mavpktword8
         }

test2 :: IO ()
test2 = do
    args <- getArgs
    case args of
        [filename] -> process filename
        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

process :: String -> IO ()
process filename = do
    inputText <- BL.readFile filename
    -- Note: Because we're not using the tree, Haskell can't infer the type of
    -- strings we're using so we need to tell it explicitly with a type signature.
    let (xml, mErr) = parse defaultParseOptions inputText :: (UNode String, Maybe XMLParseError)
    --print xml
    -- print $ onlyElems $ findChildren "enums" xml 
    print $ length $ onlyElems $ eChildren $ head $ findChildren "enums" xml
    let allEnums = getAllEnums xml
    let allMessages = getAllMessages xml
    let firstEnumEntries = getAllEntries (head allEnums)
    let secondEnumEntries = getAllEntries (last allEnums)
    let dictofmessages = [extractMsg xml | xml <- allMessages]
    let msgNames = [fromJust (MP.lookup "name" xml) | xml <- dictofmessages]
    let crcextras = [ fromJust (MP.lookup "crcextra" msg)| msg <- dictofmessages]
    let msglen = [ fromJust (MP.lookup "msglength" msg)| msg <- dictofmessages]
    -- print "******************"
    -- print allEnums
    -- print "###################"
    -- print allMessages
    -- print firstEnumEntries
    -- print "$$$$$$$$$$$$$$$$$$$$"
    -- print secondEnumEntries
    -- print "@@@@@@@@@@@@@@@@@@@@"
    -- print $ constructEnumDict allEnums
    -- print $ dictofmessages
    -- print $ get_crcextra (head dictofmessages)
    print $ (zip msgNames crcextras)
    print msglen
    -- print msgNames 
    -- print $ getData1 (head dictofmessages) ++ getData2 (head dictofmessages)
    -- Process document before handling error, so we get lazy processing.
    -- BL.hPutStr stdout $ format xml
    -- putStrLn ""
    case mErr of
        Nothing -> return ()
        Just err -> do
            hPutStrLn stderr $ "XML parse failed: "++show err
            exitWith $ ExitFailure 2


findName [] = ("name","*None*")
findName (x:xs) = if fst x == "name" then x else findName xs

constructEnumDict :: [UNode String] -> MavDictEnum
constructEnumDict xml = foldl f MP.empty xml
                        where
                            f z x = MP.insertWith (++) enumName entries z
                                where 
                                    enumName = snd $ (findName (getAttributes x))
                                    entries = getAllEntries x

getAllEnums::UNode String -> [UNode String]
getAllEnums xml = onlyElems $ eChildren $ head $ findChildren "enums" xml

getAllEntries::UNode String -> [String]
getAllEntries xml = foldl f [] (onlyElems $ eChildren xml)
                 where f z x = z ++ [snd (findName (getAttributes x))]

getAllMessages::UNode String -> [UNode String]
getAllMessages xml = onlyElems $ eChildren $ head $ findChildren "messages" xml

typeSizes:: MP.Map String Int
typeSizes = MP.fromList [("char",1),("int8_t",1), ("uint8_t",1),
                         ("int16_t",2),("uint16_t",2),
                         ("int32_t",4),("uint32_t",4),
                         ("int64_t",8),("uint64_t",8),
                         ("float",4),("double",8)]


quickSort::[(String,String)] -> [(String,String)]
quickSort [] = []
quickSort (x:xs) = quickSort [z | z <- xs, f z > f x]  ++ [x] ++ quickSort [z | z <- xs, f z <= f x]
    where f x = MP.lookup (stripArray (snd x)) typeSizes  
          stripArray value = fst (strBreak "[" value)

getMsgLength::[String] -> Int
getMsgLength typeList = foldl f 0 typeList
                      where f z x = z + _len * (fromJust (MP.lookup _type typeSizes))
                                 where 
                                  splitT = (strBreak "[" x)
                                  _type  =  fst splitT
                                  _len'  =  snd splitT
                                  _len   = if snd splitT == "" then 1 else read (init (tail _len'))


findExtensions :: [UNode String] -> [UNode String]
findExtensions [] = []
findExtensions (x:xs) = if (getName x) == "extensions" then xs else findExtensions xs

extractMsg::UNode String -> MavDictDesc
extractMsg xml = MP.fromList [("name",name),
                              ("id",id),
                              ("fields",fields),
                              ("types",types),
                              ("sortedFields",sortedFields),
                              ("sortedTypes",sortedTypes),
                              ("extensions",extensions),
                              ("msglength",msglength),
                              ("crcextra",crcextra)]
                 where
                     name    = [snd (last (getAttributes xml))]
                     id      = [snd $ head (getAttributes xml)]
                     fieldsL = findChildren "field" xml
                     fields  = [ snd ((getAttributes x)!!1) | x<-fieldsL ]
                     types   = [ snd ((getAttributes x)!!0) | x<-fieldsL ]
                     sortedFields' = [ x | x <- quickSort (zip fields types)]
                     sortedFields = fst (unzip sortedFields')
                     sortedTypes = snd (unzip sortedFields')
                     extensionsNS =  findExtensions (onlyElems (eChildren xml))
                     extensions = [ snd ((getAttributes x)!!1) | x<- extensionsNS ]
                     msglength = [show $ getMsgLength types]
                     crcextra = [show $ get_crcextra (head name) sortedFields sortedTypes extensions]


getString:: MavDictDesc -> String
getString xmldata = head $ fromJust (MP.lookup "name" xmldata)


{-
getData1::MavDictDesc -> String
getData1 xmldata = (getString xmldata) ++ " "

getData2::MavDictDesc -> String
getData2 xmldata = foldl f [] (zip fields types)
                   where
                    fields  = fromJust $ MP.lookup "sortedFields" xmldata 
                    types   = fromJust $ MP.lookup "sortedTypes" xmldata 
                    f z x   = z ++ (fieldType ++ " " ++
                             fieldName ++ " " ++ fieldLength) 
                             where
                                 fieldName = fst x
                                 fieldType = fst (strBreak "[" (snd x))
                                 lenfield  = snd (strBreak "[" (snd x))
                                 fieldLength = if length lenfield > 0
                                                  then (init (tail lenfield))
                                               else []
                                 
-}


get_crcextra :: String -> [String] -> [String] -> [String] -> Word8
get_crcextra name sortedFields sortedTypes extensions = ((fromIntegral crc) .&. 0xff) `xor` fromIntegral (crc `shiftR` 8) 
               where
                    crc     = gen_crc25 (data1 ++ data2)
                    data1   = DB.unpack $ DBC.pack(name ++ " ")
                    fields  = sortedFields
                    types   = sortedTypes
                    exten   = extensions
                    data2   = foldl f [] (zip fields types)
                    f z x   = z ++ DB.unpack (DBC.pack(fieldType ++ " " ++
                             fieldName ++ " ")) ++ fieldLength
                             where
                                 fieldName = if ((fst x) `elem` exten) then [] else (fst x)
                                 fieldType = if ((fst x) `elem` exten) then [] else (fieldtname)
                                 fieldtname = fst (strBreak "[" (snd x))
                                 lenfield   = snd (strBreak "[" (snd x))
                                 fieldLength = if (length lenfield) > 0
                                                  then [read (init (tail lenfield))]
                                               else []



    {-
get_crcextra :: MavDictDesc -> Word8
get_crcextra xmldata = ((fromIntegral crc) .&. 0xff) `xor` fromIntegral (crc `shiftR` 8) 
               where
                    crc     = gen_crc25 (data1 ++ data2)
                    data1   = DB.unpack $ DBC.pack(getString xmldata ++ " ")
                    fields  = fromJust $ MP.lookup "sortedFields" xmldata 
                    types   = fromJust $ MP.lookup "sortedTypes" xmldata 
                    exten   = fromJust $ MP.lookup "extensions" xmldata 
                    data2   = foldl f [] (zip fields types)
                    f z x   = z ++ DB.unpack (DBC.pack(fieldType ++ " " ++
                             fieldName ++ " ")) ++ fieldLength
                             where
                                 fieldName = if ((fst x) `elem` exten) then [] else (fst x)
                                 fieldType = if ((fst x) `elem` exten) then [] else (fieldtname)
                                 fieldtname = fst (strBreak "[" (snd x))
                                 lenfield   = snd (strBreak "[" (snd x))
                                 fieldLength = if (length lenfield) > 0
                                                  then [read (init (tail lenfield))]
                                               else []
                                               -}
                                 


