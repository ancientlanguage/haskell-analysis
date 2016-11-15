module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)
import Prepare.Perseus.TeiEpidocHeaderModel

data Milestone = Milestone
  { milestoneUnit :: Text
  , milestoneEd :: Text
  }
  deriving (Show)

data Content
  = ContentMilestone Milestone
  | ContentText Text
  | ContentAdd Text
  | ContentCorr Text
  | ContentDel Text
  | ContentGap
  deriving (Show)

data Section = Section
  { sectionNum :: Int
  , sectionContent :: [Content]
  }
  deriving (Show)

data Verse = Verse
  { verseNum :: Int
  , verseSections :: [Section]
  }
  deriving (Show)

data Chapter = Chapter
  { chapterNum :: Int
  , chapterVerses :: [Verse]
  }
  deriving (Show)

data Book = Book
  { bookHead :: Text
  , bookChapters :: [Chapter]
  }
  deriving (Show)

data Edition = Edition
  { editionN :: Text
  , editionLang :: Text
  }
  deriving (Show)

data Body = Body
  { bodyEdition :: Edition
  , bodyBooks :: [Book]
  }
  deriving (Show)

data TeiText = TeiText
  { teiTextLang :: Text
  , teiTextBody :: Body
  }
  deriving (Show)

data Tei = Tei
  { teiTeiHeader :: TeiHeader
  , teiTeiText :: TeiText
  }
  deriving (Show)
