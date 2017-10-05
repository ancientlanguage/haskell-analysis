{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Prepare.Xml.Parser
  ( (<|>)
  , element
  , elementNS
  , elementAttr
  , elementAttrNS
  , elementEmpty
  , elementEmptyNS
  , attribute
  , attributeValue
  , attributeNS
  , attributeXml
  , content
  , onlyContent
  , elementContent
  , elementContentNS
  , elementContentAttr
  , elementContentAttrNS
  , noAttributes
  , end
  , parseRoot
  , parseNested
  , many
  , some
  , optional
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Data.Conduit.Attoparsec (PositionRange(..), Position(..))
import Text.XML (Name(..))
import Prepare.Xml.PositionTypes

type XmlParser s a = Parsec Dec s a
type NodeParser a = XmlParser [Node] a
type AttributeParser a = XmlParser [(Name, Text)] a

data XmlError
  = XmlError
  deriving (Show)

instance Stream [Node] where
  type Token [Node] = Node
  uncons [] = Nothing
  uncons (t : ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ _ p n = (np, np)
    where np = updatePositionRangePos (getNodePosition n) p
  {-# INLINE updatePos #-}

instance Stream [(Name, Text)] where
  type Token [(Name, Text)] = (Name, Text)
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

updatePositionRangePos :: PositionRange -> SourcePos -> SourcePos
updatePositionRangePos pr pos = SourcePos (sourceName pos) line column
  where
  tryGetLineColumn p = do
    newLine <- mkPos . posLine . posRangeStart $ p
    newColumn <- mkPos . posCol . posRangeStart $ p
    return (newLine, newColumn)
  (line, column) = case tryGetLineColumn pr of
    Just (x, y) -> (x, y)
    Nothing -> (unsafePos 1, unsafePos 1)

handleToken
  :: (t -> Either e a)
  -> t
  -> Either (Set (ErrorItem t), Set y, Set z) a
handleToken f t = case f t of
  Left _ -> Left
    ( Set.singleton (Tokens (t :| []))
    , Set.empty
    , Set.empty
    )
  Right x -> Right x

tokenN
  :: MonadParsec e s m
  => (Token s -> Either XmlError a)
  -> m a
tokenN f = token (handleToken f) Nothing

contentNode :: Node -> Either XmlError Text
contentNode (NodeContent c) = Right . contentText $ c
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

elementNameNS :: Name -> Element -> Either XmlError Element
elementNameNS n e | elementName e == n = Right e
elementNameNS _ _ = Left XmlError

elementPlainNS :: Name -> NodeParser Element
elementPlainNS n = tokenN $ elementNode >=> elementNameNS n

attributeName :: Name -> (Name, Text) -> Either XmlError Text
attributeName n' (n, v) | n == n' = Right v
attributeName _ _ = Left XmlError

attributeNameValue :: Name -> Text -> (Name, Text) -> Either XmlError ()
attributeNameValue n' v' (n, v) | n == n' && v == v' = Right ()
attributeNameValue _ _ _ = Left XmlError

attribute :: Text -> AttributeParser Text
attribute = tokenN . attributeName . localName

attributeValue :: Text -> Text -> AttributeParser ()
attributeValue t v = tokenN $ attributeNameValue (localName t) v

attributeNS :: Name -> AttributeParser Text
attributeNS = tokenN . attributeName

attributeXml :: Text -> AttributeParser Text
attributeXml t = attributeNS n
  where n = Name t (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

content :: NodeParser Text
content = tokenN contentNode

onlyContent :: NodeParser Text
onlyContent = content <* end

setPositionParser :: Stream s => SourcePos -> XmlParser s a -> XmlParser s a
setPositionParser pos p = setPosition pos *> p

parseNested :: (Stream a, Stream c, ShowToken (Token a)) => String -> XmlParser a b -> a -> XmlParser c b
parseNested lab p xs = do
  pos <- getPosition
  case runParser (setPositionParser pos p) (sourceName pos) xs of
    Left e -> unexpected . Label . NonEmpty.fromList $ lab ++ " " ++ parseErrorPretty e
    Right x -> return x

elementFullNS
  :: Name
  -> AttributeParser a
  -> NodeParser c
  -> NodeParser (a, c)
elementFullNS name attributeParser childrenParser = do
  el <- elementPlainNS name
  attributeResult <- parseNested "Attribute" attributeParser (List.sort $ elementAttributes el) 
  childrenResult <- parseNested "Child" childrenParser (elementNodes el)
  return (attributeResult, childrenResult)

wrapAttributeParser :: AttributeParser a -> AttributeParser a
wrapAttributeParser p = p <* end

wrapNodeParser :: NodeParser a -> NodeParser a
wrapNodeParser p = whitespace *> p <* end

elementEmptyNS
  :: Name
  -> NodeParser ()
elementEmptyNS n = const () <$> elementFullNS n end end

elementEmpty
  :: Text
  -> NodeParser ()
elementEmpty t = elementEmptyNS (localName t)

elementAttrNS
  :: Name
  -> AttributeParser a
  -> NodeParser c
  -> NodeParser (a, c)
elementAttrNS n a c
  = elementFullNS n (wrapAttributeParser a) (wrapNodeParser c)
  <* whitespace

elementAttr
  :: Text
  -> AttributeParser a
  -> NodeParser c
  -> NodeParser (a, c)
elementAttr t a c = elementAttrNS (localName t) a c

elementContentAttrNS
  :: Name
  -> AttributeParser a
  -> NodeParser (a, Text)
elementContentAttrNS n a
  = elementFullNS n (wrapAttributeParser a) onlyContent
  <* whitespace

elementContentAttr
  :: Text
  -> AttributeParser a
  -> NodeParser (a, Text)
elementContentAttr t a = elementContentAttrNS (localName t) a

elementNS
  :: Name
  -> NodeParser c
  -> NodeParser c
elementNS n c = snd <$> elementAttrNS n noAttributes c

element 
  :: Text
  -> NodeParser c
  -> NodeParser c
element t c = elementNS (localName t) c

elementContentNS
  :: Name
  -> NodeParser Text
elementContentNS n = snd <$> elementContentAttrNS n noAttributes 

elementContent
  :: Text
  -> NodeParser Text
elementContent t = elementContentNS (localName t)

parseRoot
  :: FilePath
  -> NodeParser a
  -> Element
  -> Either String a
parseRoot file parser el = case result of
  Left x -> Left . parseErrorPretty $ x
  Right x -> Right x
  where
    result = runParser (parser <* end) file (rootList el)
    rootList = pure . NodeElement
