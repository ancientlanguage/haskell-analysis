{-# LANGUAGE OverloadedStrings #-}

module Sblgnt where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Map as Map
import Text.XML

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
  = HeadContentContent Text
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

data ParseError
  = UnexpectedNode Node
  | UnexpectedElement Element
  | UnexpectedAttribute Name Text
  | ExpectedSingleNode [Node]
  deriving (Show)

type Result a = Either [ParseError] a

local :: Text -> Name
local t = Name t Nothing Nothing

localElement :: Text -> Element -> Result Element
localElement t e | elementName e == local t = Right e
localElement _ e = Left . pure . UnexpectedElement $ e

noAttr :: Element -> Result Element
noAttr e | elementAttributes e == Map.empty = Right e
noAttr e = Left . fmap (uncurry UnexpectedAttribute) . Map.assocs . elementAttributes $ e

elementToNode :: (Element -> Result a) -> Node -> Result a
elementToNode f (NodeElement e) = f e
elementToNode _ n = Left . pure . UnexpectedNode $ n

contentNode :: Node -> Result Text
contentNode (NodeContent t) = Right t

elementNode :: Node -> Result Element
elementNode (NodeElement e) = Right e
elementNode n = Left . pure . UnexpectedNode $ n

single :: ([a] -> ParseError) -> [a] -> Result a
single _ [x] = Right x
single f xs = Left . pure . f $ xs

attributeName :: Name -> (Name, Text) -> Result Text
attributeName n (m, t) | n == m = Right t
attributeName _ (n, t) = Left . pure . UnexpectedAttribute n $ t

{-
link :: Node -> Result Link
link
  = Link . uncurry
  <$> elementNode (local "a")
  <*> (single *> attributeName (local "href"))
  <*> (single *> contentNode)
-}

parseDocument :: Element -> Result Sblgnt
parseDocument e = do
  _ <- localElement "sblgnt" e <* noAttr e
  return $ Sblgnt [] [] []
