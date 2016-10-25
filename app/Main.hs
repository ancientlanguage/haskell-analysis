module Main where

import Control.Lens (over, _1, _2)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either.Validation
import Options.Applicative hiding (Failure, Success)

import Grammar.Around
import Grammar.CommonTypes
import qualified Grammar.Greek.Script.Around as Around
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

isElided :: (a, ((b, Elision), c)) -> Bool
isElided (_, ((_, IsElided), _)) = True
isElided _ = False

queryStage
  :: Show e1
  => Stage
    (Milestone :* e1)
    e2
    [Milestone :* (String :* SentenceBoundary)]
    [b]
    [Milestone :* String]
  -> (b -> Bool)
  -> [Primary.Group]
  -> IO ()
queryStage stg f gs = mapM_ goSource $ start gs
  where
  goSource (SourceId g s, ms) = case (aroundTo $ stageAround stg) ms of
    Failure es -> Text.putStrLn $ Text.intercalate " "
      [ g
      , s
      , "Failure: aroundTo --"
      , Text.intercalate "\n" $ fmap prettyMilestoned es
      ]
    Success y -> mapM_ (Text.putStrLn . Text.append (Text.concat [ g , " ", s, " " ])) (goBack . filter f $ y)

  showItems :: [Milestone :* (String :* SentenceBoundary)] -> [Text]
  showItems = fmap prettyMilestonedString . stageForget stg

  goBack xs = case (aroundFrom $ stageAround stg) xs of
    Success ys -> showItems ys
    Failure _ -> [ "Failure: aroundFrom" ]

handleGroups :: ([Primary.Group] -> IO ()) -> IO ()
handleGroups f = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> f x

runCommand :: Options -> IO ()
runCommand (Options Words) = handleGroups showWordCounts
runCommand (Options Elision) = handleGroups (queryStage stage isElided)

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
