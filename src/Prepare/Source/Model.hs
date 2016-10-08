module Prepare.Source.Model where

import Prelude hiding (Word)
import Data.Text (Text)
import Prepare.Language

data Verse = Verse
  { verseChapter :: Integer
  , verseVerse :: Integer
  }
  deriving (Show)

data Milestone
  = MilestoneParagraph
  | MilestoneVerse Verse
  deriving (Show)

data Word = Word
  { wordPrefix :: Text
  , wordSurface :: Text
  , wordSuffix :: Text
  }
  deriving (Show)

data Content
  = ContentMilestone Milestone
  | ContentWord Word
  deriving (Show)

data Source = Source
  { sourceId :: Text
  , sourceTitle :: Text
  , sourceLicense :: [Text]
  , sourceContents :: [Content]
  }
  deriving (Show)

data Group = Group
  { groupId :: Text
  , groupLanguage :: Language
  , groupTitle :: Text
  , groupDescription :: [Text]
  , groupSources :: [Source]
  }
  deriving (Show)
