module Main where

import System.FilePath.Find
import Prepare
import Prepare.Sblgnt.Model (Sblgnt)
import qualified Prepare.Sblgnt.Unify as Sblgnt
import qualified Prepare.Source.Model as Source
import qualified Prepare.Source.Output as Output
import qualified Data.Text.IO as Text

outputSblgnt :: Sblgnt -> IO ()
outputSblgnt s = do
  let g = Sblgnt.unify s
  let gm = Output.ModuleName [ Source.groupId g, "Source", "Greek", "AncientLanguage" ]
  let sourceModules = Output.sourceModules gm (head $ Source.groupSources g)
  let baseDir = "./agda"
  mapM_ (Output.writeModule baseDir) sourceModules

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
