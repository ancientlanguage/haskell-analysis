module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types
import Prepare.Perseus.TeiEpidocModel
import Prepare.Perseus.TeiEpidocHeaderParser
import Prepare.Perseus.TeiEpidocParserCommon
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP

chapter :: NodeParser Chapter
chapter = undefined

book :: NodeParser Book
book = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, (y, z)) = Book x y z
  attributes = do
    _ <- Xml.attributeValue "subtype" "book"
    _ <- Xml.attributeValue "type" "textpart"
    n <- Xml.attribute "n"
    num <- Xml.parseNested "book number" MP.integer n
    return num
  children = do
    h <- Xml.elementContentNS (teiNS "head")
    cs <- many chapter
    return (h, cs)

edition :: NodeParser Edition
edition = build <$> Xml.elementAttrNS (teiNS "div") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    n <- Xml.attribute "n"
    _ <- Xml.attributeValue "type" "edition"
    l <- Xml.attributeXml "lang"
    return $ Edition n l

body :: NodeParser Body
body = Xml.elementNS (teiNS "body") children
  where
  children = pure Body
    <*> edition
    <*> many book

teiText :: NodeParser TeiText
teiText = build <$> Xml.elementAttrNS (teiNS "text") attributes children
  where
  build (l, b) = TeiText l b
  attributes = Xml.attributeXml "lang"
  children = body

tei :: NodeParser Tei
tei = Xml.elementNS (teiNS "TEI") children 
  where
  children = pure Tei
    <*> teiHeader
    <*> teiText
