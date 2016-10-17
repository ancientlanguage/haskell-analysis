module Main where

import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath.Find

import Prepare
import Prepare.Sblgnt.Model (Sblgnt)
import qualified Prepare.Sblgnt.Unify as Sblgnt
import qualified Primary as Primary
import qualified Prepare.Source.Output as Output

outputSblgntAgda :: Sblgnt -> IO ()
outputSblgntAgda s = do
  let g = Sblgnt.unify s
  _ <- printAffixes g
  let m = Output.groupModule g
  let baseDir = "../agda-primary/src"
  mapM_ (Output.writeModule baseDir) (Output.flatModules m)

outputSblgntBinary :: Sblgnt -> IO ()
outputSblgntBinary s = do
  let path = "../data-breakfast/groups.data"
  let g = Sblgnt.unify s
  let gs = [g]
  let encoded = Serialize.encode gs
  BS.writeFile path encoded

getWords :: Primary.Content -> [Primary.Word]
getWords (Primary.ContentWord w) = [w]
getWords (Primary.ContentMilestone _) = []

printAffixes :: Primary.Group -> IO ()
printAffixes g = do
  let sources = Primary.groupSources g
  let contents = concatMap Primary.sourceContents sources
  let words = concatMap getWords contents
  let prefixes = Set.fromList . fmap Primary.wordPrefix $ words
  let suffixes = Set.fromList . fmap Primary.wordSuffix $ words
  let printTexts = mapM_ (Text.putStrLn . (\x -> Text.concat ["\"", x, "\""])) 
  _ <- putStrLn "Prefixes"
  _ <- printTexts prefixes
  _ <- putStrLn "Suffixes"
  _ <- printTexts suffixes
  return ()

showResult :: (Sblgnt -> IO ()) -> Either String Sblgnt -> IO ()
showResult _ (Left x) = putStrLn x
showResult f (Right x) = f x 

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  -- let sblgntFile = "./examples/sblgnt-test.xml"
  result <- loadParse sblgntFile sblgnt emptyLog
  showResult outputSblgntBinary $ result

  -- let perseusDir = "./data/xml-perseus-greek"
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
