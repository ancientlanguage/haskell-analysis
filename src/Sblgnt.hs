module Sblgnt where

import Prelude hiding (Word)
import Data.Text (Text)
import Text.XML
import XmlParser

data Sblgnt = Sblgnt
  { sblgntTitle :: [HeadParagraph]
  , sblgtntLicense :: [HeadParagraph]
  , books :: [Book]
  }

data Book = Book
  { bookTitle :: Text
  , bookParagraphs :: [Paragraph]
  }

data Link = Link
  { linkHref :: Text
  , linkText :: Text
  }

data HeadParagraph = HeadParagraph
  { headParagraphContents :: [HeadContent]
  }

data HeadContent
  = HeadContentText Text
  | HeadContentLink Link

data Paragraph = Paragraph
  { paragraphContents :: [Content]
  }

data Verse = Verse
  { verseId :: Text
  , verseNumber :: Text
  }

data Content
  = ContentVerse Verse
  | ContentPrefix Text
  | ContentWord Text
  | ContentSuffix Text

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
