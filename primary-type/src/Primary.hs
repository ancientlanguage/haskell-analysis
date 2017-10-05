{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Primary where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Serialize
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

instance Serialize Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

data Language
  = Greek
  | Hebrew
  | Latin
  deriving (Show, Generic)
instance Serialize Language

data Division = Division
  { divisionBook :: Maybe Integer
  , divisionChapter :: Maybe Integer
  , divisionVerse :: Maybe Integer
  , divisionSection :: Maybe Integer
  , divisionLine :: Maybe Integer
  }
  deriving (Show, Generic)
instance Serialize Division

data Milestone
  = MilestoneParagraph
  | MilestoneDivision Division
  | MilestoneCard Integer
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
  , sourceAuthor :: Maybe Text
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
