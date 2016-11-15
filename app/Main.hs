{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath.Find

import Prepare
import Prepare.Sblgnt.Model (Sblgnt)
import qualified Prepare.Sblgnt.Unify as Sblgnt
import qualified Primary as Primary
import qualified Prepare.Source.DecomposeWords as Decompose
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
  let path = "../binary-primary/data/groups.data"
  let g = Sblgnt.unify s
  let gs = Decompose.decomposeGroups [g]
  let encoded = Serialize.encode gs
  _ <- printText ["Writing", Text.pack path] 
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

printText :: [Text] -> IO ()
printText = Text.putStrLn . Text.intercalate " "

tryParse :: FilePath -> IO Bool
tryParse xmlPath = loadParse xmlPath tei logBook >>= \case
  Left _ -> return False
  Right _ -> return True

showParsingFiles :: [FilePath] -> IO ()
showParsingFiles files = do
  results <- mapM (\x -> tryParse x >>= \y -> return (y, x)) $ files
  _ <- mapM_ (\(x, y) -> putStrLn $ (if x then "✓ " else "× ") ++ y) $ List.sort results
  putStrLn $ show (length . filter (\(x, _) -> x) $ results) ++ " files parsed"

main :: IO ()
main = do
  -- let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  -- _ <- printText ["Reading", Text.pack sblgntFile]
  -- result <- loadParse sblgntFile sblgnt emptyLog
  -- showResult outputSblgntBinary $ result

  let perseusDir = "./data/xml-perseus-greek"
  perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  let files = perseusFiles
  showParsingFiles files

  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir

  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
