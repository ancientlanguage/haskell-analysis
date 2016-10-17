{-# LANGUAGE DeriveGeneric #-}

module Prepare.Source.Model where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Serialize
import Data.Serialize.Text
import Data.Text (Text)
import Prepare.Language

data Verse = Verse
  { verseChapter :: Integer
  , verseVerse :: Integer
  }
  deriving (Show, Generic)
instance Serialize Verse 

data Milestone
  = MilestoneParagraph
  | MilestoneVerse Verse
  deriving (Show, Generic)
instance Serialize Milestone

data Word = Word
  { wordPrefix :: Text
  , wordSurface :: Text
  , wordSuffix :: Text
  }
  deriving (Show, Generic)
instance Serialize Word

data Content
  = ContentMilestone Milestone
  | ContentWord Word
  deriving (Show, Generic)
instance Serialize Content

data Source = Source
  { sourceId :: Text
  , sourceTitle :: Text
  , sourceLicense :: [Text]
  , sourceContents :: [Content]
  }
  deriving (Show, Generic)
instance Serialize Source

data Group = Group
  { groupId :: Text
  , groupLanguage :: Language
  , groupTitle :: Text
  , groupDescription :: [Text]
  , groupSources :: [Source]
  }
  deriving (Show, Generic)
instance Serialize Group
