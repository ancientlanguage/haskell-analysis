module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)
import Prepare.Perseus.TeiEpidocHeaderModel

data Milestone = Milestone
  { milestoneUnit :: Text
  , milestoneEd :: Text
  }
  deriving (Show)

data Gap = Gap
  { gapReason :: Text
  }
  deriving (Show)

data Line = Line
  { lineMet :: Maybe Text
  , lineContent :: Text
  }
  deriving (Show)

data Quote = Quote
  { quoteType :: Text
  , quoteLines :: [Line]
  }
  deriving (Show)

data Bibl = Bibl
  { biblN :: Maybe Text
  , biblContent :: Text
  }
  deriving (Show)

data Content
  = ContentMilestone Milestone
  | ContentText Text
  | ContentAdd Text
  | ContentCorr Text
  | ContentDel Text
  | ContentGap Gap
  | ContentQuote Quote
  | ContentBibl Bibl
  deriving (Show)

data Section = Section
  { sectionNum :: Integer
  , sectionContent :: [Content]
  }
  deriving (Show)

data Chapter = Chapter
  { chapterNumber :: Integer
  , chapterVerses :: [Section]
  }
  deriving (Show)

data Book = Book
  { bookNumber :: Integer
  , bookHead :: Text
  , bookChapters :: [Chapter]
  }
  deriving (Show)

data Division
  = DivisionBooks [Book]
  | DivisionChapters [Chapter]
  | DivisionSections [Section]
  deriving (Show)

data Edition = Edition
  { editionN :: Text
  , editionLang :: Text
  , editionDivision :: Division
  }
  deriving (Show)

data Body = Body
  { bodyEdition :: Edition
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
