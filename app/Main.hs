module Main where

import System.FilePath.Find
import Prepare
import Prepare.Sblgnt.Model (Sblgnt)
import qualified Prepare.Sblgnt.Output as Output
import qualified Data.Text.IO as Text

outputSblgnt :: Sblgnt -> IO ()
outputSblgnt s = do
  Text.putStrLn $ Output.sblgnt Output.emptyContext s

showResult :: Either String Sblgnt -> IO ()
showResult (Left x) = putStrLn x
showResult (Right x) = outputSblgnt x 

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
