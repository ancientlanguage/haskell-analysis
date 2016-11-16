{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Either as Either
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
import Prepare.Perseus.TeiEpidocModel (Tei)
import qualified Prepare.Perseus.TeiEpidocUnify as Tei
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

outputBinaryGroups :: [Primary.Group] -> IO ()
outputBinaryGroups gs = do
  let encoded = Serialize.encode . Decompose.decomposeGroups $ gs
  let path = "../binary-primary/data/groups.data"
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

tryParse :: FilePath -> IO (Either String Tei)
tryParse xmlPath = loadParse xmlPath tei logBook >>= \case
  Left x -> return . Left $ show x
  Right x -> return . Right $ x

showParsingFiles :: [FilePath] -> IO ()
showParsingFiles files = do
  results <- mapM (\x -> tryParse x >>= \y -> return (y, x)) $ files
  _ <- mapM_ (\(x, y) -> putStrLn $ (if x then "✓ " else "× ") ++ y) . List.sort . fmap (\(x, y) -> (Either.isRight x, y)) $ results
  putStrLn $ show (length . filter (\(x, _) -> Either.isRight x) $ results) ++ " files parsed"



main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  _ <- putStrLn "Reading files…"
--  _ <- printText ["Reading", Text.pack sblgntFile]
  sblgntResult <- (fmap . fmap) (Sblgnt.unify) $ loadParse sblgntFile sblgnt emptyLog

  let perseusDir = "./data/xml-perseus-greek"
  perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  perseusSources <- mapM ((fmap . fmap) Tei.unify . tryParse) perseusFiles
  let perseusGroup = Tei.perseusGroup { Primary.groupSources = Either.rights perseusSources }

  let
    tryAdd (Left _) xs = xs
    tryAdd (Right x) xs = x : xs
  let successful = tryAdd sblgntResult [perseusGroup]
  outputBinaryGroups successful

  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir

  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
