{-# LANGUAGE OverloadedStrings #-}

module Sblgnt where

import Prelude hiding (Word)
import Control.Monad
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
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

data ParseError
  = UnexpectedNode Node
  | UnexpectedElement Element
  | UnexpectedAttribute (Name, Text)
  | UnexpectedAttributes [(Name, Text)]
  | UnexpectedName Name
  | ExpectedSingleNode [Node]
  | ExpectedSingleAttribute [(Name, Text)]
  deriving (Show)

type Result a = Either [ParseError] a

local :: Text -> Name
local t = Name t Nothing Nothing

localName :: Text -> Name -> Result Name
localName t n | local t == n = Right n
localName _ n = Left . pure . UnexpectedName $ n

localElement :: Text -> Element -> Result Element
localElement t e | elementName e == local t = Right e
localElement _ e = Left . pure . UnexpectedElement $ e

noAttr :: Map Name Text -> Result ()
noAttr m | Map.null m = Right ()
noAttr m = Left . pure . UnexpectedAttributes . Map.assocs $ m

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

singleNode :: [Node] -> Result Node
singleNode = single ExpectedSingleNode

attributeName :: Name -> (Name, Text) -> Result Text
attributeName n (m, t) | n == m = Right t
attributeName _ a = Left . pure . UnexpectedAttribute $ a 

localAttribute :: Text -> (Name, Text) -> Result Text
localAttribute = attributeName . local

singleAttribute :: Map Name Text -> Result (Name, Text)
singleAttribute = single ExpectedSingleAttribute . Map.assocs 

elementTotal
  :: (Name -> Result a)
  -> (Map Name Text -> Result b)
  -> ([Node] -> Result c)
  -> Element
  -> Result (a, b, c)
elementTotal f g h e = do
  a <- f (elementName e)
  b <- g (elementAttributes e)
  c <- h (elementNodes e)
  return (a, b, c)

link :: Node -> Result Link
link
  = elementNode
  >=> elementTotal
    (localName "a")
    (singleAttribute >=> localAttribute "href")
    (singleNode >=> contentNode)
  >=> (\(_, b, c) -> return $ Link b c)

(<|>) :: (a -> Result b) -> (a -> Result b) -> (a -> Result b)
(<|>) f g x = case f x of
  Left e -> g x
  r@(Right _) -> r
infixl 3 <|>

manyThen :: (a -> Result b) -> (a -> Result c) -> [a] -> Result ([b], [c])
manyThen f g = fmap reversePair . List.foldl' go (Right ([], []))
  where
    reversePair (bs, cs) = (reverse bs, reverse cs)

    go (Left e) _ = Left e
    go (Right (bs, [])) a = case f a of
      Left e -> case g a of
        Left e' -> Left e'
        Right c -> return (bs, [c]) 
      Right b -> Right (b : bs, [])
    go (Right (bs, cs@(_ : _))) a = do
      c <- g a
      return (bs, c : cs)

headContent :: Node -> Result HeadContent
headContent
  = fmap HeadContentText . contentNode
  <|> fmap HeadContentLink . link

headParagraph :: Node -> Result HeadParagraph
headParagraph
  = elementNode
  >=> elementTotal
    (localName "p")
    noAttr
    (mapM headContent)
  >=> (\(_, _, xs) -> return . HeadParagraph $ xs)

headParagraphs :: [Node] -> Result [HeadParagraph]
headParagraphs = mapM headParagraph 

parseDocument :: Element -> Result Sblgnt
parseDocument e = e `seq` return $ Sblgnt [] [] []
