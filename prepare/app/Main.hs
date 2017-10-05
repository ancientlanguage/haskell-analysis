{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map.Strict as Map
import System.Environment

import qualified Greek
import qualified Hebrew

dumpCommands :: [String] -> IO ()
dumpCommands keys = do
  _ <- putStrLn "Commands:"
  mapM_ putStrLn keys

main :: IO ()
main = do
  let
    allCommands = Map.union
      (Map.mapKeys ("greek-"++) Greek.commands)
      (Map.mapKeys ("hebrew-"++) Hebrew.commands)
  let help = dumpCommands $ Map.keys allCommands
  args <- getArgs
  case args of
    [x] -> case Map.lookup x allCommands of
      Just f -> f
      Nothing -> help
    _ -> help
