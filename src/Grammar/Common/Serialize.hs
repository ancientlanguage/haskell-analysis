module Grammar.Common.Serialize where

import Control.Lens (over, _1, _2)
import Control.Monad (filterM)
import Control.Monad.Extra (concatMapM)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Common.List
import Grammar.Common.Numeric
import Grammar.Common.Types
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path
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
  maxDig = maxDigits . fmap fst $ xs
  go = over (traverse . _1) (prefix </>) . fmap (\(x, (y, z)) -> (filePathWithIndex maxDig x y, z))

saveAsFile :: Serialize a => (FilePath -> IO ()) -> FilePath -> a -> IO ()
saveAsFile doFirst path item = do
  _ <- doFirst path
  BS.writeFile path (Serialize.encode item)

saveStage' :: Serialize a => String -> (FilePath -> IO ()) -> FilePath -> [SourceId :* a] -> IO ()
saveStage' ext doFirst prefix = mapM_ saveGroup . prefixFilePaths prefix . splitIndexSourceIds
  where
  saveGroup
    :: Serialize a
    => FilePath :* [Int :* Text :* a]
    -> IO ()
  saveGroup (g, ss) = do
    _ <- Dir.createDirectoryIfMissing True g
    let items = over (traverse . _1) (flip Path.addExtension ext) . prefixFilePaths g $ ss
    mapM_ (uncurry $ saveAsFile doFirst) $ items

printFileName :: FilePath -> IO ()
printFileName p = Text.putStrLn $ Text.intercalate " " ["Writing", Text.pack p]

dataExtension :: String
dataExtension = "data"

saveStage :: Serialize a => FilePath -> [SourceId :* a] -> IO ()
saveStage = saveStage' dataExtension printFileName

pathToId :: FilePath -> Text
pathToId = Text.pack . stripPrefix . Path.takeBaseName
  where
  stripPrefix x = case List.elemIndex '_' x of
    Just i -> drop (i + 1) x
    Nothing -> x

loadSource :: Serialize a => Text -> FilePath -> IO [SourceId :* a]
loadSource g p = do
  bin <- BS.readFile p
  let maybeItem = Serialize.decode bin
  case maybeItem of
    Right x -> return [(SourceId g (pathToId p), x)]
    Left e -> do
      _ <- putStrLn $ "Unable to decode " ++ p ++ "\n" ++ e
      return []

loadGroup :: Serialize a => FilePath -> IO [SourceId :* a]
loadGroup p = do
  contents <- Dir.listDirectory p
  allFiles <- filterM Dir.doesFileExist . fmap (p </>) $ contents
  let files = List.sort $ filter (\x -> Path.takeExtension x == ".data") allFiles
  concatMapM (loadSource (pathToId p)) files

loadStage :: Serialize a => FilePath -> IO [SourceId :* a]
loadStage dir = do
  contents <- Dir.listDirectory dir
  subdirs <- filterM Dir.doesDirectoryExist . fmap (dir </>) $ contents
  let sorted = List.sort subdirs
  groups <- mapM loadGroup sorted
  return $ concat groups

verifyLoadStage :: (Eq a, Serialize a) => FilePath -> [SourceId :* a] -> IO ()
verifyLoadStage dataPath ss' = do
  _ <- putStrLn "Verifying load…"
  ss'' <- loadStage dataPath
  case ss'' == ss' of
    True -> putStrLn "Success"
    False -> do
      _ <- putStrLn "Loaded stage doesn't match"
      let showSource ((SourceId g s), _) = Text.unpack $ Text.intercalate " " [g, s]
      _ <- putStrLn $ "saved:" ++ List.intercalate "\n" (fmap showSource ss')
      _ <- putStrLn $ "loaded:" ++ List.intercalate "\n" (fmap showSource ss'')
      return ()
