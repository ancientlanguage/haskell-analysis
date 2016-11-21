{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map.Strict as Map
import System.Environment

import qualified Greek

dumpCommands :: [String] -> IO ()
dumpCommands keys = do
  _ <- putStrLn "Commands:"
  mapM_ putStrLn keys

main :: IO ()
main = do
  let allCommands = Greek.commands
  let help = dumpCommands $ Map.keys allCommands
  args <- getArgs
  case args of
    [x] -> case Map.lookup x allCommands of
      Just f -> f
      Nothing -> help
    _ -> help
