{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Prelude hiding (Word)
import Data.Semigroup ((<>))
import qualified Data.Text.Format as Lazy
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import Options.Applicative hiding (Failure, Success)

import Grammar.IO.QueryStage
import Grammar.Common.Types
import Grammar.Common.Numeric
import qualified Grammar.Greek.Morph.Serialize as Serialize

import Morph (showMorphForms)

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
  | CommandRun Query
  | CommandList
  | CommandMorph QueryOptions
  | CommandSave

options :: Parser Command
options = subparser
  ( command "sources"
    ( info
      (pure CommandSources)
      (progDesc "Show info for primary sources" )
    )
  <> command "run"
    ( info
      (CommandRun <$> queryParser)
      (progDesc "Run query" )
    )
  <> command "list"
    ( info
      (pure CommandList)
      (progDesc "List available queries" )
    )
  <> command "morph"
    ( info
      (pure CommandMorph <*> queryOptionsParser)
      (progDesc "Show morph ending pairs")
    )
  <> command "save"
    ( info
      (pure CommandSave)
      (progDesc "Save current analysis" )
    )
  )

showSourceId :: SourceId -> Lazy.Text
showSourceId (SourceId g s) = Lazy.format "{} {}" (Lazy.fromStrict g, Lazy.fromStrict s)

showWordCounts :: IO ()
showWordCounts = do
  ss <- Serialize.readScript
  let pairs = fmap toLengthPair ss
  let maxIdLength = fromIntegral . maximum $ fmap (Lazy.length . fst) pairs
  let maxDig = maxDigits . fmap snd $ pairs
  mapM_ (printColumns maxIdLength maxDig) pairs
  where
  toLengthPair (sid, ms) = (showSourceId sid, length ms)
  printColumns llen rlen (x, y) = Lazy.putStrLn $ Lazy.format "{} {} words" (Lazy.right llen ' ' x, Lazy.left rlen ' ' y)

runCommand :: Command -> IO ()
runCommand (CommandSources) = showWordCounts
runCommand (CommandRun (Query _ _)) = putStrLn "Hang tight—no queries yet!"
runCommand (CommandList) = putStrLn "Hang tight—no queries yet!"
runCommand (CommandMorph opt) = showMorphForms opt
runCommand (CommandSave) = putStrLn "Hang tight—nothing to save yet!"

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query"
    )
