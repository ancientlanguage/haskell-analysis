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
import Xml.PositionTypes
import Xml.Events (readRootElement)

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
  attributeResult <- parseNested attributeParser "Attribute" pos (elementAttributes element) 
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
  elementResult <- readRootElement file
  case elementResult of 
    Left e -> return . Left . show $ e
    Right x -> return $ parseNodes file parser x 
