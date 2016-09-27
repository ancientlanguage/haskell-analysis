module Prepare.Log where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.XML as XML
import Prepare.Xml.PositionTypes

showName :: XML.Name -> Text
showName (XML.Name ln _ Nothing) = ln
showName (XML.Name ln _ (Just p)) = Text.concat [p, ":", ln]

showAttribute :: (XML.Name, Text) -> Text
showAttribute (n, t) = Text.concat [showName n, "=\"", t, "\""]

showAttributes :: [(XML.Name, Text)] -> Text
showAttributes = Text.intercalate " " . fmap showAttribute

logElement :: (Text -> Bool) -> Element -> IO ()
logElement p e
  | n <- XML.nameLocalName . elementName $ e
  , p n
  , as <- elementAttributes e
  = Text.putStrLn $ Text.intercalate " " [n, showAttributes as]
logElement _ _ = return ()

logBook :: Element -> IO ()
logBook = logElement (== "book")
