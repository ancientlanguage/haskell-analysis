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
import qualified Prepare.Source.Model as Source
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

getWords :: Source.Content -> [Source.Word]
getWords (Source.ContentWord w) = [w]
getWords (Source.ContentMilestone _) = []

printAffixes :: Source.Group -> IO ()
printAffixes g = do
  let sources = Source.groupSources g
  let contents = concatMap Source.sourceContents sources
  let words = concatMap getWords contents
  let prefixes = Set.fromList . fmap Source.wordPrefix $ words
  let suffixes = Set.fromList . fmap Source.wordSuffix $ words
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
