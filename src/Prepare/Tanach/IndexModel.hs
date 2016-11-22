module Prepare.Tanach.IndexModel where

import Data.Text (Text)
import Prepare.Tanach.TeiHeaderModel (TeiHeader)

data Chapter = Chapter
  { chapterNumber :: Integer
  , chapterVerseCount :: Integer
  }
  deriving (Show)

data Name = Name
  { nameName :: Text
  , nameAbbrev :: Text
  , nameFilename :: Text
  }
  deriving (Show)

data Book = Book
  { bookName :: Name
  , bookChapters :: [Chapter]
  , bookChapterCount :: Integer
  }
  deriving (Show)

data Index = Index
  { indexHeader :: TeiHeader
  , indexBooks :: [Book]
  }
  deriving (Show)
