{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Xml.Parser
  ( (<|>)
  , element
  , elementAttr
  , elementEmpty
  , attribute
  , content
  , onlyContent
  , elementContent
  , elementContentAttr
  , noAttributes
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

type XmlParser s a = Parsec Dec s a
type NodeParser a = XmlParser [Node] a
type AttributeParser a = XmlParser [(Name, Text)] a

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

showsTokenFromShow :: Show a => NonEmpty a -> String
showsTokenFromShow = List.intercalate " " . fmap show . NonEmpty.toList 

instance ShowToken Node where showTokens = showsTokenFromShow
instance ShowToken (Name, Text) where showTokens = showsTokenFromShow

end :: Stream s => XmlParser s ()
end = eof

noAttributes :: AttributeParser ()
noAttributes = end

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

localElementName :: Text -> Element -> Either XmlError Element
localElementName n e | elementName e == localName n = Right e
localElementName _ _ = Left XmlError

elementPlain :: Text -> NodeParser Element
elementPlain t = tokenN $ elementNode >=> localElementName t

attributeName :: Text -> (Name, Text) -> Either XmlError Text
attributeName t (n, v) | n == localName t = Right v
attributeName _ _ = Left XmlError

attribute :: Text -> AttributeParser Text
attribute = tokenN . attributeName

content :: NodeParser Text
content = tokenN contentNode

onlyContent :: NodeParser Text
onlyContent = content <* end

setPositionParser :: Stream s => SourcePos -> XmlParser s a -> XmlParser s a
setPositionParser pos p = setPosition pos *> p

parseNested :: (Stream a, Stream c, ShowToken (Token a)) => XmlParser a b -> String -> SourcePos -> a -> XmlParser c b
parseNested p label pos xs = case runParser (setPositionParser pos p) (sourceName pos) xs of
    Left e -> unexpected . Label . NonEmpty.fromList $ label ++ " " ++ parseErrorPretty e
    Right x -> return x

elementFull
  :: Text
  -> AttributeParser a
  -> NodeParser c
  -> NodeParser (a, c)
elementFull name attributeParser childrenParser = do
  element <- elementPlain name
  pos <- getPosition
  let orderedAttributes = Map.assocs . elementAttributes $ element
  attributeResult <- parseNested attributeParser "Attribute" pos orderedAttributes 
  childrenResult <- parseNested childrenParser "Child" pos (elementNodes element)
  return (attributeResult, childrenResult)

wrapAttributeParser :: AttributeParser a -> AttributeParser a
wrapAttributeParser p = p <* end

wrapNodeParser :: NodeParser a -> NodeParser a
wrapNodeParser p = whitespace *> p <* end 

elementEmpty
  :: Text
  -> NodeParser ()
elementEmpty t = const () <$> elementFull t end end

elementAttr
  :: Text
  -> AttributeParser a
  -> NodeParser c
  -> NodeParser (a, c)
elementAttr t a c
  = elementFull t (wrapAttributeParser a) (wrapNodeParser c)
  <* whitespace

elementContentAttr
  :: Text
  -> AttributeParser a
  -> NodeParser (a, Text)
elementContentAttr t a
  = elementFull t (wrapAttributeParser a) onlyContent
  <* whitespace

element 
  :: Text
  -> NodeParser c
  -> NodeParser c
element t c = snd <$> elementAttr t noAttributes c

elementContent
  :: Text
  -> NodeParser Text
elementContent t = snd <$> elementContentAttr t noAttributes 

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
