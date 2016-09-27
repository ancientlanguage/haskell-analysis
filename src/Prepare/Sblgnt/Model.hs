module Prepare.Sblgnt.Model where

import Prelude hiding (Word)
import Data.Text (Text)

data Sblgnt = Sblgnt
  { sblgntTitle :: [HeadParagraph]
  , sblgtntLicense :: [HeadParagraph]
  , books :: [Book]
  }
  deriving (Show)

data Book = Book
  { bookId :: Text
  , bookTitle :: Text
  , bookParagraphs :: [Paragraph]
  , bookMarkEnd :: Maybe MarkEnd
  }
  deriving (Show)

data Link = Link
  { linkHref :: Text
  , linkText :: Text
  }
  deriving (Show)

data HeadParagraph = HeadParagraph
  { headParagraphContents :: [HeadContent]
  }
  deriving (Show)

data HeadContent
  = HeadContentText Text
  | HeadContentLink Link
  deriving (Show)

data Ending = Ending
  { endingTitle :: Text
  , endingParagraphs :: [Paragraph]
  }
  deriving (Show)

data MarkEnd = MarkEnd
  { markEndTitle :: Text
  , markEndEndings :: [Ending]
  }
  deriving (Show)

data Paragraph = Paragraph
  { paragraphContents :: [Content]
  }
  deriving (Show)

data Verse = Verse
  { verseId :: Text
  , verseNumber :: Text
  }
  deriving (Show)

data Word = Word
  { wordPrefix :: Maybe Text
  , wordText :: Text
  , wordSuffix :: Maybe Text
  }
  deriving (Show)

data Content
  = ContentVerse Verse
  | ContentWord Word
  deriving (Show)
