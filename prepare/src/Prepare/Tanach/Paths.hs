module Prepare.Tanach.Paths where

import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))
import Prepare.Tanach.IndexModel (Index)
import qualified Prepare.Tanach.IndexModel as Index

tanachBasePath :: FilePath -> FilePath
tanachBasePath dataPath = dataPath </> "xml-tanach/books/"

indexFilePath :: FilePath -> FilePath
indexFilePath dataPath = tanachBasePath dataPath </> "TanachIndex.xml"

headerFilePath :: FilePath -> FilePath
headerFilePath dataPath = tanachBasePath dataPath </> "TanachHeader.xml"

getAllFilePaths :: FilePath -> Index -> [FilePath]
getAllFilePaths dataPath idx = fmap ((\x -> tanachBasePath dataPath </> (Text.unpack x) <.> ".xml") . Index.nameFilename . Index.bookName)
  $ Index.indexBooks idx
