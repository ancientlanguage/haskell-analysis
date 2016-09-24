module Sblgnt where

import Prelude hiding (Word)
import Data.Text (Text)
import Xml.Parser

data Sblgnt = Sblgnt
  { sblgntTitle :: [HeadParagraph]
  , sblgtntLicense :: [HeadParagraph]
  , books :: [Book]
  }
  deriving (Show)

data Book = Book
  { bookTitle :: Text
  , bookParagraphs :: [Paragraph]
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

data Paragraph = Paragraph
  { paragraphContents :: [Content]
  }
  deriving (Show)

data Verse = Verse
  { verseId :: Text
  , verseNumber :: Text
  }
  deriving (Show)

data Content
  = ContentVerse Verse
  | ContentPrefix Text
  | ContentWord Text
  | ContentSuffix Text
  deriving (Show)

link :: NodeParser Link
link = (uncurry Link) <$> elementContentAttr "a" (attribute "href")

headContent :: NodeParser HeadContent
headContent
  = HeadContentText <$> content
  <|> HeadContentLink <$> link

headParagraph :: NodeParser HeadParagraph
headParagraph = HeadParagraph <$> some headContent

headParagraphList :: NodeParser [HeadParagraph]
headParagraphList = some headParagraph

title :: NodeParser [HeadParagraph]
title = element "title" headParagraphList

license :: NodeParser [HeadParagraph]
license = element "license" headParagraphList

sblgnt :: NodeParser ([HeadParagraph], [HeadParagraph])
sblgnt = (const ([], [])) <$> elementEmpty "sblgnt"
