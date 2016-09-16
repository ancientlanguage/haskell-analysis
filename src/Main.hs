module Main where

import Sblgnt
import qualified Text.XML as XML

main :: IO ()
main = do
  sblgntDocument <- XML.readFile XML.def "./data/xml-sblgnt/sblgnt.xml"
  let parsedSblgnt = parseDocument (XML.documentRoot sblgntDocument)
  case parsedSblgnt of
    Left x -> print x
    Right _ -> putStrLn "Success!"
