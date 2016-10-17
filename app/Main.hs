module Main where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize
import Primary
import Lib

decodeGroups :: BS.ByteString -> Either String [Group]
decodeGroups = Serialize.decode

main :: IO ()
main = do
  let path = "../data-breakfast/groups.data"
  encoded <- BS.readFile path
  case decodeGroups encoded of
    Left x -> putStrLn x
    Right x -> putStrLn . show . sum . fmap (length . groupSources) $ x
