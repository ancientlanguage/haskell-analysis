module Hebrew where

import Data.Map (Map)
import qualified Data.Map as Map
import Prepare
import Prepare.Tanach.IndexParser (index)
import Prepare.Tanach.HeaderParser (header)
import Prepare.Tanach.TanachParser (tanach)
import qualified Prepare.Tanach.Paths as Paths
import System.FilePath ((</>))

loadIndex :: IO ()
loadIndex = do
  result <- loadParse Paths.indexFilePath index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right _ -> putStrLn "Success!"

loadHeader :: IO ()
loadHeader = do
  result <- loadParse Paths.headerFilePath header emptyLog
  case result of
    Left e -> putStrLn $ "Error loading header:\n" ++ e
    Right _ -> putStrLn "Success!"

showLoadSingle :: FilePath -> IO ()
showLoadSingle p = do
  result <- loadParse p tanach emptyLog
  case result of
    Left e -> putStrLn $ "✗ " ++ p ++ "\n" ++ e
    Right _ -> putStrLn $ "✓ " ++ p

loadSingle :: IO ()
loadSingle = showLoadSingle $ Paths.tanachBasePath </> "Amos.xml"

loadAll :: IO ()
loadAll = do
  result <- loadParse Paths.indexFilePath index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right idx ->
      mapM_ showLoadSingle $ Paths.getAllFilePaths idx

commands :: Map String (IO ())
commands = Map.fromList
  [ ("load-index", loadIndex)
  , ("load-header", loadHeader)
  , ("load-single", loadSingle)
  , ("load-all", loadAll)
  ]
