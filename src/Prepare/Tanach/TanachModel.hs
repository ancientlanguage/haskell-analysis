module Prepare.Tanach.TanachModel where

import Prelude hiding (Word)
import Data.Text (Text)
import Prepare.Tanach.TeiHeaderModel (TeiHeader)
import Prepare.Tanach.HeaderModel (Note)
import Prepare.Tanach.IndexModel (Name)

data WordSize = WordSize
  { wordSizeType :: Text
  , wordSizeValue :: Text
  }
  deriving (Show)

data WordContent
  = WordText Text
  | WordX Text
  | WordS WordSize
  deriving (Show)

data Word = Word [WordContent]
  deriving (Show)

data Milestone
  = MilestonePe
  | MilestoneSamekh
  | MilestoneReversedNun
  deriving (Show)

data VerseContent
  = VerseMilestone Milestone
  | VerseWord Word
  | VerseWordK Word
  | VerseWordQ Word
  | VerseX Text
  deriving (Show)

data Verse = Verse
  { verseNumber :: Integer
  , verseContents :: [VerseContent]
  }
  deriving (Show)

data Chapter = Chapter
  { chapterNumber :: Integer
  , chapterVerses :: [Verse]
  , chapterVerseCount :: Integer
  }
  deriving (Show)

data Book = Book
  { bookName :: Name
  , bookChapters :: [Chapter]
  , bookVerseCount :: Integer
  , bookChapterCount :: Integer
  }
  deriving (Show)

data Tanach = Tanach
  { tanachTeiHeader :: TeiHeader
  , tanachBook :: Book
  , tanachNotes :: [Note]
  }
  deriving (Show)
