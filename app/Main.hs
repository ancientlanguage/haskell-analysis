module Main where

import System.FilePath.Find
import qualified Sblgnt.Parser as Parser
import Xml.Events
import Xml.PositionTypes
import Xml.Parser
import Log

logBook :: Element -> IO ()
logBook = logElement (== "book")

loadParseFile :: Show a => FilePath -> NodeParser a -> IO ()
loadParseFile file parser = readRootElement logBook file >>= \case
  Right root -> case parseRoot file parser root of
    Left e -> putStrLn $ "XML Parse Error:\n" ++ e
    Right x -> putStrLn $ "Success!" 
  Left e -> putStrLn $ "XML Load Error: " ++ file ++ " -- " ++ show e

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  -- let sblgntFile = "./examples/sblgnt-test.xml"
  loadParseFile sblgntFile Parser.sblgnt

  -- let perseusDir = "./data/xml-perseus-greek"
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
