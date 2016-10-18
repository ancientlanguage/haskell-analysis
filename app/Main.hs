module Main where

import Grammar.Serialize
import Primary

main :: IO ()
main = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> putStrLn . show . sum . fmap (length . groupSources) $ x
