module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either.Validation
import Options.Applicative hiding (Failure, Success)

import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Stage
import Grammar.Greek.Script.Types
import Grammar.Prepare
import Grammar.Pretty
import Grammar.Serialize
import qualified Primary

data Command
  = Words
  | Elision

data Options = Options
  { optCommand :: Command
  }

sourcesOptions :: Parser Options
sourcesOptions = pure $ Options Words

elisionOptions :: Parser Options
elisionOptions = pure $ Options Elision

options :: Parser Options
options = subparser
  ( command "sources" (info sourcesOptions
    ( progDesc "Show info for primary sources" ))
  <> command "elision" (info elisionOptions
    ( progDesc "Show words with elision" ))
  )

showWordCounts :: [Primary.Group] -> IO ()
showWordCounts x = mapM_ showGroup x
  where
  showGroup g = mapM_ (showSource (Primary.groupId g)) (Primary.groupSources g) 
  showSource g s = Text.putStrLn $ Text.intercalate " "
    [ g
    , Primary.sourceId s
    , "—"
    , textShow . length . filter filterWords $ Primary.sourceContents s
    , "words"
    ]
  filterWords (Primary.ContentWord w) = True
  filterWords _ = False

showElision :: (Show a, Show b) => [Milestone :* (a :* Elision) :* b] -> [Text]
showElision = fmap prettyMilestoned . filter isElided
  where
  isElided (_, ((_, IsElided), _)) = True
  isElided _ = False

queryStage f gs = do
  let stageTo = aroundTo $ stageAround stage
  let ss = start gs
  let
    goSource (SourceId g s, ms) = case stageTo ms of
      Failure es -> Text.putStrLn $ Text.intercalate " "
        [ g
        , s
        , "to failure:"
        , Text.intercalate "\n" $ fmap prettyMilestoned es
        ]
      Success y -> mapM_ (Text.putStrLn . Text.append (Text.concat [ g , " ", s, " " ])) $ f y
  mapM_ goSource ss

handleGroups :: ([Primary.Group] -> IO ()) -> IO ()
handleGroups f = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> f x

runCommand :: Options -> IO ()
runCommand (Options Words) = handleGroups showWordCounts
runCommand (Options Elision) = handleGroups (queryStage showElision)

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
