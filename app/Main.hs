{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (words)
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
import Prepare.Perseus.Paths (perseusShortList)
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

tryParseTei :: FilePath -> IO (Either String Tei)
tryParseTei xmlPath = loadParse xmlPath tei logBook

showParsingFiles :: [FilePath] -> IO ()
showParsingFiles files = do
  results <- mapM (\x -> tryParseTei x >>= \y -> return (y, x)) $ files
  _ <- mapM_ (\(x, y) -> putStrLn $ (if x then "✓ " else "× ") ++ y) . List.sort . fmap (\(x, y) -> (Either.isRight x, y)) $ results
  putStrLn $ show (length . filter (\(x, _) -> Either.isRight x) $ results) ++ " files parsed"

getPrimaryWords :: Primary.Content -> [Primary.Word]
getPrimaryWords (Primary.ContentWord w) = [w]
getPrimaryWords (Primary.ContentMilestone _) = []

dumpAffixes :: [Primary.Group] -> IO ()
dumpAffixes gs = do
  _ <- putStrLn "Prefixes: "
  mapM_ (Text.putStrLn . Text.append "  ") prefixes
  _ <- putStrLn ""
  _ <- putStrLn "Suffixes: "
  mapM_ (Text.putStrLn . Text.append "  ") suffixes
  where
  prefixes
    = Set.fromList
    . fmap Primary.wordPrefix
    $ words
  suffixes
    = Set.fromList
    . fmap Primary.wordSuffix
    $ words
  words
    = concatMap getPrimaryWords
    . concatMap Primary.sourceContents
    . concatMap Primary.groupSources
    $ gs

dumpInvalidWords :: [Primary.Group] -> IO ()
dumpInvalidWords gs = mapM_ dumpInvalids $ concatMap Primary.groupSources gs
  where
  dumpInvalids s =
    let invalids = getInvalids (concatMap getPrimaryWords $ Primary.sourceContents s)
    in
      if List.null invalids
      then return ()
      else do
        _ <- Text.putStrLn (Primary.sourceId s)
        mapM_ (Text.putStrLn . Text.append "  ") invalids
  getInvalids
    = Set.fromList
    . fmap (\(Primary.Word x y z) -> Text.intercalate " || " [x, y, z])
    . filter isInvalid
  isInvalid (Primary.Word p t s)
    = Text.null t
    || not (Text.all isCore t)
    || Text.any isGreekChar p
    || Text.any isGreekChar s
  isGreekChar x 
    = x /= '\x037e' -- Greek question mark
    && ((x >= '\x0370' && x <= '\x03ff')
      || (x >= '\x1f00' && x <= '\x1fff'))
  isCore x = isGreekChar x || x == '\x2019' -- apostrophe

loadAllGroups :: IO [Primary.Group]
loadAllGroups = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  _ <- printText ["Reading", Text.pack sblgntFile]
  sblgntResult <- (fmap . fmap) (Sblgnt.unify) $ loadParse sblgntFile sblgnt emptyLog

  let
    parseUnify x = do
      _ <- putStrLn $ "Reading " ++ x
      t <- (fmap . fmap) Tei.unify . tryParseTei $ x
      _ <- case t of
        Left e -> putStrLn $ "  " ++ e
        Right r -> Text.putStrLn $ Text.concat 
          [ "  "
          , Maybe.maybe "-" id . Primary.sourceAuthor $ r
          , " -- "
          , Text.intercalate " " . Text.words . Primary.sourceTitle $ r
          ]
      return t
  perseusSources <- mapM parseUnify perseusShortList
  let perseusGroup = Tei.perseusGroup { Primary.groupSources = Either.rights perseusSources }

  let
    tryAdd (Left _) xs = xs
    tryAdd (Right x) xs = x : xs
  let successful = tryAdd sblgntResult [perseusGroup]
  return successful

showAllLoadResults :: [FilePath] -> IO ()
showAllLoadResults files = do
   let
     handleSingle x = do
       _ <- putStrLn ""
       _ <- putStrLn x
       y <- tryParseTei x
       case y of
         Left e -> putStrLn $ "ERROR: " ++ take 1000 e
         Right _ -> putStrLn $ "SUCCESS"
   mapM_ handleSingle files

showSingleLoadResult :: FilePath -> IO ()
showSingleLoadResult file =
  tryParseTei file >>= \case
    Left e -> putStrLn $ e
    Right _ -> putStrLn "Success!"

main :: IO ()
main = do
  successful <- loadAllGroups
  -- dumpAffixes successful
  -- dumpInvalidWords successful
  outputBinaryGroups successful

  -- let perseusDir = "./data/xml-perseus-greek"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- showSingleLoadResult "./data/xml-perseus-greek/data/tlg0001/tlg001/tlg0001.tlg001.perseus-grc2.xml"
