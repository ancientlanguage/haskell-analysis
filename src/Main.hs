module Main where

import Xml.Events
import System.FilePath.Find

loadFile :: FilePath -> IO ()
loadFile file = do
  result <- readRootElement file
  case result of
    Right _ -> putStrLn $ "Success: " ++ file
    Left x -> putStrLn $ "Error: " ++ file ++ " -- " ++ show x

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  let perseusDir = "./data/xml-perseus-greek"
  perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  let files = sblgntFile : perseusFiles
  mapM_ loadFile files
