module Main where

import Options.Applicative
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
showWordCounts x = putStrLn . show . sum . fmap (length . groupSources) $ x

showElision :: [Group] -> IO ()
showElision _ = putStrLn "elision"

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
