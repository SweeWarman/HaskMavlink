module Main where

import XmlParser
import System.Environment
import System.Exit
import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processXmlFile filename
        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

