module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)
import Prepare.Perseus.TeiEpidocHeaderModel

data Milestone
  = MilestoneParagraph { milestoneParagraphEd :: Text }
  | MilestoneCard { milestoneCardN :: Integer }
  deriving (Show)

data Gap = Gap
  { gapReason :: Maybe Text
  }
  deriving (Show)

data QuoteLine = QuoteLine
  { quoteLineMet :: Maybe Text
  , quoteLineContent :: Text
  , quoteLineAna :: Maybe Text
  }
  deriving (Show)

data Quote = Quote
  { quoteType :: Text
  , quoteLines :: [QuoteLine]
  }
  deriving (Show)

data Bibl = Bibl
  { biblN :: Maybe Text
  , biblContent :: Text
  , biblDefault :: Maybe Text
  }
  deriving (Show)

data Cit = Cit
  { citQuote :: Quote
  , citBibl :: Bibl
  }
  deriving (Show)

data ApparatusDel = ApparatusDel Text
  deriving (Show)

data ApparatusCorr = ApparatusCorr Text
  deriving (Show)

data ApparatusAdd = ApparatusAdd Text
  deriving (Show)

data Term = Term Text
  deriving (Show)

data Content
  = ContentMilestone Milestone
  | ContentText Text
  | ContentAdd ApparatusAdd
  | ContentCorr ApparatusCorr
  | ContentDel ApparatusDel
  | ContentTerm Term
  | ContentGap Gap
  | ContentQuote Quote
  | ContentBibl Bibl
  | ContentCit Cit
  deriving (Show)

data LineRender = LineRender_DisplayNumAndIndent
  deriving (Show)

data LineContent
  = LineContentMilestone Milestone
  | LineContentText Text
  | LineContentDel ApparatusDel
  deriving (Show)

data Line = Line
  { lineNumber :: Maybe Integer
  , lineRend :: Maybe LineRender
  , lineContents :: [LineContent]
  }
  deriving (Show)

data BookLineContent
  = BookLineContentMilestone Milestone
  | BookLineContentLine Line
  deriving (Show)

data Section = Section
  { sectionNum :: Integer
  , sectionContent :: [Content]
  }
  deriving (Show)

data Chapter = Chapter
  { chapterNumber :: Integer
  , chapterSections :: [Section]
  }
  deriving (Show)

data Book = Book
  { bookNumber :: Integer
  , bookHead :: Maybe Text
  , bookChapters :: [Chapter]
  }
  deriving (Show)

data BookLines = BookLines
  { bookLinesNumber :: Integer
  , bookLinesLineContents :: [BookLineContent]
  }
  deriving (Show)

data Division
  = DivisionBooks [Book]
  | DivisionChapters [Chapter]
  | DivisionSections [Section]
  | DivisionBookLines [BookLines]
  deriving (Show)

data Edition = Edition
  { editionN :: Text
  , editionLang :: Maybe Text
  , editionDivision :: Division
  }
  deriving (Show)

data Body
  = BodyEdition Edition
  | BodyDivision Division
  deriving (Show)

data Interp = Interp
  { interpId :: Text
  , interpValue :: Text
  }
  deriving (Show)

data InterpGrp = InterpGrp
  { interpGrpType :: Text
  , interpGrpLang :: Text
  , interpGrpInterps :: [Interp]
  }
  deriving (Show)

data TeiText = TeiText
  { teiTextLang :: Maybe Text
  , teiTextBody :: Body
  , teiTextN :: Maybe Text
  , teiTextIntrpGrp :: Maybe InterpGrp
  }
  deriving (Show)

data Tei = Tei
  { teiTeiHeader :: TeiHeader
  , teiTeiText :: TeiText
  }
  deriving (Show)
