module Prepare
  ( loadParse
  , logBook
  , emptyLog
  )
  where

import Prelude hiding (log)
import Prepare.Xml.Events
import Prepare.Xml.Parser
import Prepare.Xml.PositionTypes
import Prepare.Log

loadParse
  :: FilePath
  -> NodeParser a
  -> (Element -> IO ())
  -> IO (Either String a)
loadParse file parser log = do
  rootResult <- readRootElement log file
  let
    finalResult = case rootResult of
      Right root -> case parseRoot file parser root of
        Left e -> Left $ "XML Parse Error:\n" ++ e
        Right x -> Right x 
      Left e -> Left $ "XML Load Error: " ++ file ++ "\n" ++ show e
  return finalResult
