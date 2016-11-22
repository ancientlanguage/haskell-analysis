module Hebrew where

import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Prepare
import Prepare.Tanach.IndexParser (index)
import qualified Prepare.Tanach.IndexModel as Index
import Prepare.Tanach.HeaderParser (header)
import Prepare.Tanach.TanachParser (tanach)

indexFilePath :: FilePath
indexFilePath = "./data/xml-tanach/books/TanachIndex.xml"

headerFilePath :: FilePath
headerFilePath = "./data/xml-tanach/books/TanachHeader.xml"

loadIndex :: IO ()
loadIndex = do
  result <- loadParse indexFilePath index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right _ -> putStrLn "Success!"

loadHeader :: IO ()
loadHeader = do
  result <- loadParse headerFilePath header emptyLog
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
loadSingle = showLoadSingle "./data/xml-tanach/books/Amos.xml"

loadAll :: IO ()
loadAll = do
  result <- loadParse indexFilePath index emptyLog
  case result of
    Left e -> putStrLn $ "Error loading index:\n" ++ e
    Right idx ->
      mapM_ showLoadSingle
      . fmap ((\x -> "./data/xml-tanach/books/" ++ (Text.unpack x) ++ ".xml") . Index.nameFilename . Index.bookName)
      $ Index.indexBooks idx

commands :: Map String (IO ())
commands = Map.fromList
  [ ("load-index", loadIndex)
  , ("load-header", loadHeader)
  , ("load-single", loadSingle)
  , ("load-all", loadAll)
  ]
