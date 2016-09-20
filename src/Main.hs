module Main where

import Sblgnt
import XmlParser (readParse)

main :: IO ()
main = do
  let file = "./data/xml-sblgnt/sblgnt.xml"
  parsedSblgnt <- readParse file sblgnt
  case parsedSblgnt of
    Left x -> print x
    Right (x,y,zs) -> putStrLn $ (show . length $ zs) ++ " books"
