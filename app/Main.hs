module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.XML as XML
import System.FilePath.Find
import qualified Sblgnt.Parser as Parser
import Xml.Events
import Xml.PositionTypes
import Xml.Parser

showName :: XML.Name -> Text
showName (XML.Name ln _ Nothing) = ln
showName (XML.Name ln _ (Just p)) = Text.concat [p, ":", ln]

showAttribute :: (XML.Name, Text) -> Text
showAttribute (n, t) = Text.concat [showName n, "=\"", t, "\""]

showAttributes :: [(XML.Name, Text)] -> Text
showAttributes = Text.intercalate " " . fmap showAttribute

logElement :: Element -> IO ()
logElement e
  | n <- XML.nameLocalName . elementName $ e
  , n == "title" || n == "book"
  , as <- elementAttributes e
  = Text.putStrLn $ Text.intercalate " " [n, showAttributes as]
logElement _ = return ()

loadParseFile :: Show a => FilePath -> NodeParser a -> IO ()
loadParseFile file parser = readRootElement logElement file >>= \case
  Right root -> case parseRoot file parser root of
    Left e -> putStrLn $ "XML Parse Error:\n" ++ e
    Right x -> putStrLn $ "Success!" 
  Left e -> putStrLn $ "XML Load Error: " ++ file ++ " -- " ++ show e

main :: IO ()
main = do
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  -- let sblgntFile = "./examples/sblgnt-test.xml"
  loadParseFile sblgntFile Parser.sblgnt

  -- let perseusDir = "./data/xml-perseus-greek"
  -- let papyriDir = "./data/xml-papyri/DDB_EpiDoc_XML/"
  -- perseusFiles <- find always (fileName ~~? "*-grc*.xml") perseusDir
  -- papyriFiles <- find always (fileName ~~? "*.xml") papyriDir
  -- let files = sblgntFile : perseusFiles ++ papyriFiles
  -- mapM_ loadFile files
