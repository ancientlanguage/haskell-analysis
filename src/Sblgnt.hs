module Sblgnt where

import Prelude hiding (Word)
import Data.Text (Text)
import Xml.Parser (NodeParser, (<|>), some)
import qualified Xml.Parser as Xml

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
link = (uncurry Link) <$> Xml.elementContentAttr "a" (Xml.attribute "href")

headContent :: NodeParser HeadContent
headContent
  = HeadContentText <$> Xml.content
  <|> HeadContentLink <$> link

headParagraph :: NodeParser HeadParagraph
headParagraph = HeadParagraph <$> Xml.element "p" (some headContent)

headParagraphList :: NodeParser [HeadParagraph]
headParagraphList = some headParagraph

headTitle :: NodeParser [HeadParagraph]
headTitle = Xml.element "title" headParagraphList

license :: NodeParser [HeadParagraph]
license = Xml.element "license" headParagraphList

title :: NodeParser Text
title = Xml.element "title" Xml.content

paragraph :: NodeParser Text
paragraph = Xml.element "p" Xml.content

book :: NodeParser Book
book = build <$> Xml.elementAttr "book" attributes children
  where
  build (i, (t, ps)) = Book i t ps
  attributes = do
    i <- Xml.attribute "id"
    return i
  children = do
    t <- title
    return $ (t, [])

sblgnt :: NodeParser Sblgnt
sblgnt = Xml.element "sblgnt" children
  where
  children = do
    t <- headTitle
    l <- license
    bs <- some book
    return $ Sblgnt t l bs
