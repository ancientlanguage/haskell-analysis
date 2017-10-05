module Hebrew where

import Data.Map (Map)
import qualified Data.Map as Map
import Prepare
import Prepare.Tanach.IndexParser (index)
import Prepare.Tanach.HeaderParser (header)
import Prepare.Tanach.TanachParser (tanach)
import qualified Prepare.Tanach.Paths as Paths
import System.FilePath ((</>))

loadIndex :: FilePath -> IO ()
loadIndex dataPath = do
  result <- loadParse (Paths.indexFilePath dataPath) index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right _ -> putStrLn "Success!"

loadHeader :: FilePath -> IO ()
loadHeader dataPath = do
  result <- loadParse (Paths.headerFilePath dataPath) header emptyLog
  case result of
    Left e -> putStrLn $ "Error loading header:\n" ++ e
    Right _ -> putStrLn "Success!"

showLoadSingle :: FilePath -> IO ()
showLoadSingle p = do
  result <- loadParse p tanach emptyLog
  case result of
    Left e -> putStrLn $ "✗ " ++ p ++ "\n" ++ e
    Right _ -> putStrLn $ "✓ " ++ p

loadSingle :: FilePath -> IO ()
loadSingle dataPath = showLoadSingle $ Paths.tanachBasePath dataPath </> "Amos.xml"

loadAll :: FilePath -> IO ()
loadAll dataPath = do
  result <- loadParse (Paths.indexFilePath dataPath) index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right idx ->
      mapM_ showLoadSingle $ (Paths.getAllFilePaths dataPath) idx

commands :: FilePath -> Map String (IO ())
commands dataPath = Map.fromList
  [ ("load-index", loadIndex dataPath)
  , ("load-header", loadHeader dataPath)
  , ("load-single", loadSingle dataPath)
  , ("load-all", loadAll dataPath)
  ]
