module Grammar.SerializeStage where

import Control.Lens (over, _1, _2)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.CommonTypes
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), addExtension)
import Text.Printf (printf)

splitIndexSourceIds
  :: [SourceId :* a]
  -> [Int :* Text :* [Int :* Text :* a]]
splitIndexSourceIds = addIndex . over (traverse . _2) addIndex . groupConsecutivePairs . fmap splitSourceId
  where
  splitSourceId (SourceId gid sid, a) = (gid, (sid, a))

filePathWithIndex :: Int -> Int -> Text -> FilePath
filePathWithIndex dig i n = Text.unpack $ Text.concat [Text.pack . printf ("%0" ++ show dig ++ "d") $ i, "_", n]

prefixFilePaths :: FilePath -> [Int :* Text :* a] -> [FilePath :* a]
prefixFilePaths prefix xs = go xs
  where
  maxDigitsDouble :: Double
  maxDigitsDouble = logBase 10 . fromIntegral . maximum . (0 :) . fmap fst $ xs
  maxDigits :: Int
  maxDigits = ceiling maxDigitsDouble
  go = over (traverse . _1) (prefix </>) . fmap (\(x, (y, z)) -> (filePathWithIndex maxDigits x y, z))

serializeAsFile :: Serialize a => (FilePath -> IO ()) -> FilePath -> a -> IO ()
serializeAsFile doFirst path item = do
  _ <- doFirst path
  BS.writeFile path (Serialize.encode item)

serializeStage' :: Serialize a => String -> (FilePath -> IO ()) -> FilePath -> [SourceId :* a] -> IO ()
serializeStage' ext doFirst prefix = mapM_ saveGroup . prefixFilePaths prefix . splitIndexSourceIds
  where
  saveGroup
    :: Serialize a
    => FilePath :* [Int :* Text :* a]
    -> IO ()
  saveGroup (g, ss) = do
    _ <- createDirectoryIfMissing True g
    let items = over (traverse . _1) (flip addExtension ext) . prefixFilePaths g $ ss
    mapM_ (uncurry $ serializeAsFile doFirst) $ items

printFileName :: FilePath -> IO ()
printFileName p = Text.putStrLn $ Text.intercalate " " ["Writing", Text.pack p]

dataExtension :: String
dataExtension = "data"

serializeStage :: Serialize a => FilePath -> [SourceId :* a] -> IO ()
serializeStage = serializeStage' dataExtension printFileName
