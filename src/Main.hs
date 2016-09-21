module Main where

import Sblgnt
import XmlParser (readParse)

main :: IO ()
main = do
--  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  let sblgntFile = "./examples/sblgnt-test.xml"
  parsedSblgnt <- readParse sblgntFile sblgnt
  case parsedSblgnt of
    Left x -> print x
    Right (xs, ys) -> putStrLn . show . length $ xs
