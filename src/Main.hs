module Main where

import System.FilePath.Find
import qualified Sblgnt as Parser
import Xml.Events
import Xml.Parser

loadFile :: FilePath -> IO ()
loadFile file = do
  result <- readRootElement file
  case result of
    Right _ -> return () -- putStrLn $ "Success: " ++ file
    Left x -> putStrLn $ "Error: " ++ file ++ " -- " ++ show x

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  sblgnt <- readParse sblgntFile Parser.sblgnt
  case sblgnt of
    Left x -> putStrLn $ "Error: " ++ show x
    Right x -> putStrLn $ "Success!"

  -- let perseusDir = "./data/xml-perseus-greek"
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
