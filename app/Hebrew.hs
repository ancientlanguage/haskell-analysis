module Hebrew where

import Data.Map (Map)
import qualified Data.Map as Map
import Prepare
import Prepare.Tanach.TeiHeaderParser (tanach)

indexFilePath :: FilePath
indexFilePath = "./data/xml-tanach/books/TanachIndex.xml"

loadIndex :: IO ()
loadIndex = do
  result <- loadParse indexFilePath tanach logBook
  case result of
    Left e -> putStrLn $ "Error:\n" ++ e
    Right _ -> putStrLn "Success!"

commands :: Map String (IO ())
commands = Map.fromList
  [ ("load-index", loadIndex)
  ]
