module Prepare.Tanach.Paths where

import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))
import Prepare.Tanach.IndexModel (Index)
import qualified Prepare.Tanach.IndexModel as Index

tanachBasePath :: FilePath
tanachBasePath = "./data/xml-tanach/books/"

indexFilePath :: FilePath
indexFilePath = tanachBasePath </> "TanachIndex.xml"

headerFilePath :: FilePath
headerFilePath = tanachBasePath </> "TanachHeader.xml"

getAllFilePaths :: Index -> [FilePath]
getAllFilePaths idx = fmap ((\x -> tanachBasePath </> (Text.unpack x) <.> ".xml") . Index.nameFilename . Index.bookName)
  $ Index.indexBooks idx
