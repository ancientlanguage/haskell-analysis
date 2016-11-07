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
import System.FilePath ((</>))

splitIndexSourceIds
  :: [SourceId :* a]
  -> [Int :* Text :* [Int :* Text :* a]]
splitIndexSourceIds = addIndex . over (traverse . _2) addIndex . groupConsecutivePairs . fmap splitSourceId
  where
  splitSourceId (SourceId gid sid, a) = (gid, (sid, a))

filePathWithIndex :: Int -> Text -> FilePath
filePathWithIndex i n = Text.unpack $ Text.concat [Text.pack . show $ i, "_", n]

prefixFilePaths :: FilePath -> [Int :* Text :* a] -> [FilePath :* a]
prefixFilePaths prefix = over (traverse . _1) (prefix </>) . fmap (\(x, (y, z)) -> (filePathWithIndex x y, z))

printFileName :: FilePath -> IO ()
printFileName p = Text.putStrLn $ Text.intercalate " " ["Writing", Text.pack p]

serializeAsFile :: Serialize a => (FilePath -> IO ()) -> FilePath -> a -> IO ()
serializeAsFile doFirst path item = do
  _ <- doFirst path
  BS.writeFile path (Serialize.encode item)

serializeStage' :: Serialize a => (FilePath -> IO ()) -> FilePath -> [SourceId :* a] -> IO ()
serializeStage' doFirst prefix = mapM_ saveGroup . prefixFilePaths prefix . splitIndexSourceIds
  where
  saveGroup
    :: Serialize a
    => FilePath :* [Int :* Text :* a]
    -> IO ()
  saveGroup (g, ss) = do
    _ <- createDirectoryIfMissing True g
    mapM_ (uncurry $ serializeAsFile doFirst) $ prefixFilePaths g ss

serializeStage :: Serialize a => FilePath -> [SourceId :* a] -> IO ()
serializeStage = serializeStage' printFileName
