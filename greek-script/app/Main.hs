{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Prelude hiding (Word)
import Control.Lens (over, _2)
import Data.Either.Validation
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative hiding (Failure, Success)

import qualified ScriptQueries
import Grammar.IO.QueryStage
import Grammar.Common.Prepare
import Grammar.Common.Pretty
import Grammar.Common.Round
import Grammar.Common.Types
import qualified Grammar.IO.Serialize as Serialize
import qualified Grammar.Greek.Script.Stage as Stage
import Grammar.Greek.Script.Word (Word)
import qualified Grammar.Greek.Script.Serialize as Serialize
import qualified Primary

queryOptionsParser :: Parser QueryOptions
queryOptionsParser = QueryOptions <$> resultOptionParser <*> matchParser <*> omitParser <*> contextParser
  where
  resultOptionParser :: Parser ResultOption
  resultOptionParser = option auto
    ( long "results"
    <> short 'r'
    <> value Summary
    <> metavar "{Summary | All | First N | Random N}"
    )

  matchParser = strOption
    ( long "match"
    <> short 'm'
    <> value ""
    <> metavar "HEADING"
    )

  omitParser = strOption
    ( long "omit"
    <> short 'o'
    <> value ""
    <> metavar "HEADING"
    )

  contextParser = option auto
    ( long "context"
    <> short 'c'
    <> value 5
    <> metavar "{number of words of context in results}"
    )

data Query = Query
  { queryName :: String
  , queryOptions :: QueryOptions
  }

queryParser :: Parser Query
queryParser = Query <$> name <*> queryOptionsParser
  where
  name = strArgument
    ( metavar "NAME"
    <> help "Query name"
    )

data Command
  = CommandSources
  | CommandScriptQuery Query
  | CommandList String
  | CommandSave

options :: Parser Command
options = subparser
  ( command "sources"
    ( info
      (pure CommandSources)
      (progDesc "Show info for primary sources" )
    )
  <> command "script"
    ( info
      (CommandScriptQuery <$> queryParser)
      (progDesc "Query for script properties" )
    )
  <> command "list"
    ( info
      (CommandList <$> (strArgument (value "" <> metavar "C" <> help "Property category")))
      (progDesc "List available queries" )
    )
  <> command "save"
    ( info
      (pure CommandSave)
      (progDesc "Save script analysis" )
    )
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
  filterWords (Primary.ContentWord _) = True
  filterWords _ = False

handleGroups :: FilePath -> ([Primary.Group] -> IO ()) -> IO ()
handleGroups modulesPath f = do
  result <- Serialize.readGroups modulesPath
  case result of
    Left x -> putStrLn x
    Right x -> f x

queryCategories :: Map String (Map String (QueryOptions -> [Primary.Group] -> IO ()))
queryCategories = Map.fromList
  [ ("script", ScriptQueries.queries)
  ]

showCategory :: String -> IO ()
showCategory c = do
  case Map.lookup c queryCategories of
    Just m -> do
      mapM_ (\x -> putStrLn $ c ++ " " ++ x) $ Map.keys m
    Nothing -> putStrLn $ "Invalid query category: " ++ c

saveScript :: [Primary.Group] -> IO ()
saveScript gs = case (traverse . _2) (roundTo Stage.script . over (traverse . _2) Stage.basicWord) . prepareGroups $ gs of
  Failure es -> mapM_ (putStrLn . show) es
  Success (ss' :: [SourceId :* [Milestone :* Word]]) -> do
    let dataPath = "../binary-greek-script/data"
    _ <- Serialize.saveStage dataPath ss'
    Serialize.verifyLoadStage dataPath ss'

runCommand :: FilePath -> Command -> IO ()
runCommand modulesPath (CommandSources) = handleGroups modulesPath showWordCounts
runCommand modulesPath (CommandScriptQuery (Query n opt)) = case Map.lookup n ScriptQueries.queries of
  Just f -> handleGroups modulesPath (f opt)
  Nothing -> putStrLn $ "Invalid query name: " ++ n
runCommand _ (CommandList "") = mapM_ showCategory $ Map.keys queryCategories
runCommand _ (CommandList c) = showCategory c
runCommand modulesPath (CommandSave) = handleGroups modulesPath saveScript

main :: IO ()
main = do
  let
    -- modulesPath = "./modules"
    modulesPath = ".."
  execParser opts >>= runCommand modulesPath
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
