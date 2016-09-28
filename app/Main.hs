module Main where

import System.FilePath.Find
import Prepare

showResult :: Either String a -> IO ()
showResult (Left x) = putStrLn x
showResult (Right _) = putStrLn "Success!"

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  -- let sblgntFile = "./examples/sblgnt-test.xml"
  result <- loadParse sblgntFile sblgnt emptyLog
  showResult result

  -- let perseusDir = "./data/xml-perseus-greek"
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
