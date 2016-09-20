module Main where

import Sblgnt
import qualified Text.XML as XML

main :: IO ()
main = do
  let file = "./data/xml-sblgnt/sblgnt.xml"
  sblgntDocument <- XML.readFile XML.def file 
  let parsedSblgnt = parseSblgnt file (XML.documentRoot sblgntDocument)
  case parsedSblgnt of
    Left x -> print x
    Right (x,y,zs) -> putStrLn $ (show . length $ zs) ++ " books"
