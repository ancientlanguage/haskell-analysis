module Main where

import Xml.Events
import System.FilePath.Find

loadFile :: FilePath -> IO ()
loadFile file = do
  result <- readRootElement file
  case result of
    Right _ -> return () -- putStrLn $ "Success: " ++ file
    Left x -> putStrLn $ "Error: " ++ file ++ " -- " ++ show x

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  let perseusDir = "./data/xml-perseus-greek"
  let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  let files = sblgntFile : perseusFiles ++ papyriFiles
  mapM_ loadFile files
