module Main where


import Options.Applicative
import Data.Semigroup ((<>))
import XmlParser
import System.Environment
import System.Exit
import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)

data InputOpts = InputOpts
    { xmlfilename :: String
    , outputdir   :: String }


inputOpts::Parser InputOpts
inputOpts = InputOpts <$> strOption (short 'i' <> help "input .xml file containing mavlink message definition")
                      <*> strOption (short 'o' <> help "output directory to store generated modules")



runProcess :: InputOpts -> IO ()
runProcess (InputOpts fname odir) = processXmlFile fname odir


main:: IO ()
main = execParser opts >>= runProcess
    where
        opts = info (inputOpts <**> helper)
                   ( fullDesc 
                   <> progDesc "Generate haskell module from given mavlink xml definition"
                   <> header "mavgen - a haskell library generator for mavlink")

