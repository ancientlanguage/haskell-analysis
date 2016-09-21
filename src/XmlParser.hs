{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module XmlParser
  ( (<|>)
  , element
  , end
  , readParse
  , some
  , whitespace
  , NodeParser
  , AttributeParser
  )
where

import Prelude hiding (readFile)
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
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.XML as XML

type NodeParser a = Parsec Dec [Node] a
type AttributeParser a = Parsec Dec [(Name, Text)] a

data XmlError
  = XmlError
  deriving (Show)

instance Ord a => Stream [a] where
  type Token [a] = a
  uncons [] = Nothing
  uncons (t : ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ _ p _ = (p, p)
  {-# INLINE updatePos #-}

instance ShowToken Node where
  showTokens = List.intercalate " " . fmap show . NonEmpty.toList

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

whitespace :: NodeParser ()
whitespace = skipMany . tokenN $ contentNode >=> test
  where
    test t | Text.all Char.isSpace t = Right ()
    test _ = Left XmlError

localName :: Text -> Name
localName t = Name t Nothing Nothing

elementNode :: Node -> Either XmlError Element
elementNode (NodeElement e) = Right e
elementNode _ = Left XmlError

elementLocal :: Text -> Element -> Either XmlError Element
elementLocal n e | elementName e == localName n = Right e
elementLocal _ _ = Left XmlError

elementPlain :: Text -> NodeParser Element
elementPlain t = tokenN $ elementNode >=> elementLocal t

attribute :: Text -> (Name, Text) -> Either XmlError Text
attribute t (n, v) | n == localName t = Right v
attribute _ _ = Left XmlError

element :: Text -> NodeParser Element
element t = elementPlain t <* whitespace

end :: NodeParser ()
end = eof

parseNodes
  :: FilePath
  -> NodeParser a
  -> Element
  -> Either String a
parseNodes file parser el = case result of
  Left x -> Left . parseErrorPretty $ x
  Right x -> Right x
  where
    result = runParser parser file (elementNodes $ el)

readParse
  :: FilePath
  -> NodeParser a
  -> IO (Either String a)
readParse file parser = do
  document <- readFile def file 
  return $ parseNodes file parser (documentRoot document)
