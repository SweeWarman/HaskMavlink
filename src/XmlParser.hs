module XmlParser (
    processXmlFile
)where 

import Data.Binary.Get
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Strings
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as MP
import MavlinkHelper
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

-- Function to process a given mavlink xml file
-- and generate message and enum definitions and associated helper function 
processXmlFile :: String -> IO ()
processXmlFile filename = do
    inputText <- BL.readFile filename
    -- Note: Because we're not using the tree, Haskell can't infer the type of
    -- strings we're using so we need to tell it explicitly with a type signature.
    let (xml, mErr) = parse defaultParseOptions inputText :: (UNode String, Maybe XMLParseError)
    let allEnums = getAllEnums xml
    let allMessages = getAllMessages xml
    let dictofenums  = constructEnumDict allEnums
    let dictofmessages = [extractMsg xml | xml <- allMessages]
    let msgString = fmap generateMsgString dictofmessages 
    let allMsgString = foldl (++) "" msgString
    let enumString = generateEnums dictofenums
    let outputString = ("\n" ++ allMsgString ++ "\n\n\n-- ENUMS\n\n" ++ enumString)
    let headerString = "module Icarous where\n\n"
    let importString = "import Data.Binary.Get\n" ++ 
                       "import Data.Int\n" ++ 
                       "import MavlinkHelper\n" ++ 
                       "import qualified Data.ByteString.Lazy as BS\n\n" 
    writeFile "src/Icarous.hs" (headerString ++ importString ++ outputString)
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
typeSizes = MP.fromList [("char",1),("int8_t",1), ("uint8_t",1),("uint8_t_mavlink_version",1),
                         ("int16_t",2),("uint16_t",2),
                         ("int32_t",4),("uint32_t",4),
                         ("int64_t",8),("uint64_t",8),
                         ("float",4),("double",8)]

typeConversion::MP.Map String String
typeConversion = MP.fromList [("char","Char"),("int8_t","Int8"), ("uint8_t","Uint8"),("uint8_t_mavlink_version","Uint8"),
                         ("int16_t","Int16"),("uint16_t","Uint16"),
                         ("int32_t","Int32"),("uint32_t","Uint32"),
                         ("int64_t","Int64"),("uint64_t","Uint64"),
                         ("float","Float"),("double","Double")]

typeGetMonad::MP.Map String String
typeGetMonad = MP.fromList [("char","getInt8"),("int8_t","getInt8"), ("uint8_t","getWord8"),("uint8_t_mavlink_version","getWord8"),
                         ("int16_t","getInt16le"),("uint16_t","getWord16le"),
                         ("int32_t","getInt32le"),("uint32_t","getWord32le"),
                         ("int64_t","getInt64le"),("uint64_t","getWord64le"),
                         ("float","getFloatle"),("double","getDoublele")]




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


typecombinator::(Int,String)->(String,String)->(Int,String)
typecombinator (n,z) (field,typed) = (n-1,z ++ "      " ++ field ++ "::" ++ typetext ++ comma ++ "\n")
                               where
                                   t = strBreak "[" typed
                                   typeN = fst t
                                   len = length (snd t) > 0 
                                   ts = (fromJust (MP.lookup typeN typeConversion))
                                   typetext = if len then "[" ++ ts ++ "]" else ts
                                   comma = if n > 1 then "," else ""


generateMsgDataType::MavDictDesc -> String
generateMsgDataType msgdata =  firstLine ++ (snd typesList) ++ "}\n\n\n"
                               where
                                   firstLine = "data " ++ name ++ " = " ++ name ++ "{\n"
                                   name' = head $ fromJust (MP.lookup "name" msgdata)
                                   name = strCapitalize (strToLower name')
                                   types = fromJust (MP.lookup "sortedTypes" msgdata)
                                   fields = fromJust (MP.lookup "sortedFields" msgdata)
                                   typesList = foldl typecombinator (length types,"") (zip fields types)


fieldCombinator::(Int,String) -> (String,String) -> (Int,String)
fieldCombinator (n,z) (field,typed) = (n-1,z ++ "     _" ++ field ++ " <- " ++ typemonadS ++ ";\n") 
                                    where
                                        t = strBreak "[" typed
                                        typeN = fst t
                                        len = length (snd t) > 0
                                        numV = (read (init (tail (snd t)))) 
                                        typeL = if len then numV else 0 
                                        typemonad = fromJust $ (MP.lookup typeN typeGetMonad)
                                        typemonadS = if typeL == 0 then typemonad
                                                     else "mapM (\\x ->" ++ typemonad ++ ") [ i | i <- [1.." ++ show(typeL) ++ "] ]"


generateDecoder::MavDictDesc -> String
generateDecoder msgdata = typeLine ++ firstLine ++ (snd fieldsText) ++ lastline ++ "\n}\n\n\n"
                           where
                             typeLine = "decode" ++ name ++ "::Get " ++ name ++ "\n"
                             firstLine = "decode" ++ name ++ "= do {\n"
                             name' = head $ fromJust (MP.lookup "name" msgdata)
                             name = strCapitalize (strToLower name')
                             types = fromJust (MP.lookup "sortedTypes" msgdata)
                             fields = fromJust (MP.lookup "sortedFields" msgdata)
                             fieldsText = foldl fieldCombinator (length types,"")  (zip fields types)
                             lastline =  foldl f ("     return $ " ++ name ++ " ") fields
                             f z x = z ++ " _" ++ x


generateDecoderWrapper::MavDictDesc -> String
generateDecoderWrapper msgdata = typeLine ++ firstLine ++ restline ++ "\n"
                                where
                                    typeLine = "get" ++ name ++ "::Mavlink2Pkt -> " ++ name ++ "\n"
                                    firstLine = "get" ++ name ++ " mavpkt = runGet decode" ++ name  ++ " (BS.pack fullPayload)\n"
                                    name' = head $ fromJust (MP.lookup "name" msgdata)
                                    nameL = (strToLower name')
                                    name = strCapitalize nameL
                                    restline = "    where\n" ++ 
                                               "        truncPayload = payload mavpkt\n" ++
                                               "        lenPayload = length truncPayload\n" ++ 
                                               "        fullPayload = if lenPayload < " ++ nameL ++ "_len then\n" ++ 
                                               "                          truncPayload ++ [ 0 | i <- [1..(" ++ nameL ++"_len - lenPayload)] ]\n" ++  
                                               "                      else truncPayload"


walkEnumList:: String -> (String,[String]) -> String
walkEnumList z dataList = z ++ firstline  ++ (snd entries) ++ "\n\n"
                        where
                            firstline = "data " ++ (fst dataList) ++ " =\n"
                            len = length (snd dataList)
                            entries = foldl f (len,"") (snd dataList)
                            f z x = (n-1,(snd z) ++ "     " ++ x ++ if n == 1 then "\n" else " | \n")
                                    where
                                        n = fst z

generateEnums::MavDictEnum -> String
generateEnums msgdata = descp 
                   where
                       enumlist = MP.toList msgdata
                       descp = foldl walkEnumList "" enumlist


generateMsgString:: MavDictEnum -> String
generateMsgString msgdata = preamble ++ msgidS ++ lenString ++ crcextraString ++ msgType ++ decstring ++ decwrapstring 
     where 
       preamble = "\n\n-- message: " ++ name ++ "\n\n"
       lenString = nameL ++ "_len = " ++ msglen ++ "\n\n"
       msgType = generateMsgDataType msgdata
       decstring = generateDecoder (msgdata)
       decwrapstring = generateDecoderWrapper (msgdata)
       msglen = head $ fromJust (MP.lookup "msglength" msgdata)
       name = head $ fromJust  (MP.lookup "name" msgdata)
       nameL = strToLower name
       msgidS = nameL ++ "_id = " ++ (head $ fromJust (MP.lookup "id" msgdata)) ++ "\n\n"
       crcextra = head $ fromJust  (MP.lookup "crcextra" msgdata) 
       crcextraString = nameL ++ "_crc_extra = " ++ crcextra ++ "\n\n" 
