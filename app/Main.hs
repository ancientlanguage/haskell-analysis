module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either.Validation
import Options.Applicative hiding (Failure, Success)

import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Stage
import Grammar.Prepare
import Grammar.Pretty
import Grammar.Serialize
import Primary

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

showWordCounts :: [Group] -> IO ()
showWordCounts x = mapM_ showGroup x
  where
  showGroup g = mapM_ (showSource (groupId g)) (groupSources g) 
  showSource g s = Text.putStrLn $ Text.intercalate " "
    [ g
    , sourceId s
    , "—"
    , textShow . length . filter filterWords $ sourceContents s
    , "words"
    ]
  filterWords (ContentWord w) = True
  filterWords _ = False

showElision :: [Group] -> IO ()
showElision gs = do
  let stageTo = aroundTo $ stageAround stage
  let ss = start gs
  let
    goSource f (SourceId g s, ms) = case f ms of
      Failure es -> Text.putStrLn $ Text.intercalate " "
        [ g
        , s
        , "to failure:"
        , Text.concat $ fmap (Text.append "\n" . prettyMilestoned) es
        ]
      Success y -> Text.putStrLn $ Text.intercalate " " [ g, s, "Success!" ]
  mapM_ (goSource stageTo) ss

handleGroups :: ([Group] -> IO ()) -> IO ()
handleGroups f = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> f x

runCommand :: Options -> IO ()
runCommand (Options Words) = handleGroups showWordCounts
runCommand (Options Elision) = handleGroups showElision

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
