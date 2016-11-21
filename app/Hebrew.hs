module Hebrew where

import Data.Map (Map)
import qualified Data.Map as Map
import Prepare

indexFilePath :: FilePath
indexFilePath = "./data/xml-tanach/books/TanachIndex.xml"

loadIndex :: IO ()
loadIndex = putStrLn "Hello Hebrew!"

commands :: Map String (IO ())
commands = Map.fromList
  [ ("load-index", loadIndex)
  ]
