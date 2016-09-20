{-# LANGUAGE OverloadedStrings #-}

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

sblgnt :: XmlParser (Element, Element, [Element])
sblgnt = do
  _ <- whitespace
  title <- element "title" <* whitespace 
  license <- element "license" <* whitespace
  books <- some (element "book" <* whitespace)
  _ <- end
  return $ (title, license, books)
