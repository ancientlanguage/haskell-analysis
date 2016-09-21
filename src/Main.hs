module Main where

import Sblgnt
import XmlEvents
import XmlParser (readParse)

main :: IO ()
main = do
  let file = "./examples/sblgnt-test.xml"
  events <- readEvents file
  putStrLn . show $ events
{-
  let sblgntFile = "./data/xml-sblgnt/sblgnt.xml"
  parsedSblgnt <- readParse sblgntFile sblgnt
  case parsedSblgnt of
    Left x -> print x
    Right (xs, ys) -> putStrLn . show . length $ xs
-}
