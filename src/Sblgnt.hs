{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Sblgnt where

import Prelude hiding (Word)
import Control.Applicative
import Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.XML
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

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

data XmlError
  = XmlError
  deriving (Show)

tokenN
  :: MonadParsec e s m
  => (Token s -> Either XmlError a)
  -> m a
tokenN f = token g Nothing
  where
    g t = case f t of
      Left e -> Left
        ( Set.singleton (Tokens (t :| []))
        , Set.empty
        , Set.empty
        )
      Right x -> Right x

contentNode :: Node -> Either XmlError Text
contentNode (NodeContent t) = Right t
contentNode _ = Left XmlError

whitespace :: (MonadParsec e s m, Token s ~ Node) => m ()
whitespace = skipMany . tokenN $ contentNode >=> test
  where
    test t | Text.all Char.isSpace t = Right ()
    test _ = Left XmlError

elementNode :: Node -> Either XmlError Element
elementNode (NodeElement e) = Right e
elementNode _ = Left XmlError

elementLocal :: Text -> Element -> Either XmlError Element
elementLocal n e | elementName e == Name n Nothing Nothing = Right e
elementLocal _ _ = Left XmlError

element :: (MonadParsec e s m, Token s ~ Node) => Text -> m Element
element t = tokenN $ elementNode >=> elementLocal t

sblgnt :: (MonadParsec e s m, Token s ~ Node) => m (Element, Element, [Element])
sblgnt = do
  _ <- whitespace
  title <- element "title" <* whitespace 
  license <- element "license" <* whitespace
  books <- some (element "book" <* whitespace)
  _ <- eof
  return $ (title, license, books)

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False

parseSblgnt :: FilePath -> Element -> Either String (Element, Element, [Element])
parseSblgnt file e = case result of
  Left x -> Left . parseErrorPretty $ x
  Right x -> Right x
  where
    result :: Either (ParseError Node Dec) (Element, Element, [Element])
    result = runParser sblgnt file (List.filter isElement . elementNodes $ e)

instance Stream [Node] where
  type Token [Node] = Node
  uncons [] = Nothing
  uncons (t : ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ _ p _ = (p, p)
  {-# INLINE updatePos #-}

instance ShowToken Node where
  showTokens = List.intercalate " " . fmap show . NonEmpty.toList
