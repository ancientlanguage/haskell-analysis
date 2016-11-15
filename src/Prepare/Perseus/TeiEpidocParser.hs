module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types (Name)
import Prepare.Perseus.TeiEpidocModel
import Prepare.Perseus.TeiEpidocHeaderParser
import Prepare.Perseus.TeiEpidocParserCommon
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP

milestone :: NodeParser Milestone
milestone = build <$> Xml.elementAttrNS (teiNS "milestone") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    ed <- Xml.attribute "ed"
    u <- Xml.attribute "unit"
    return $ Milestone u ed

contentAdd :: NodeParser Content
contentAdd = ContentAdd <$> Xml.elementContentNS (teiNS "add")

contentDel :: NodeParser Content
contentDel = ContentDel <$> Xml.elementContentNS (teiNS "del")

contentCorr :: NodeParser Content
contentCorr = ContentCorr <$> Xml.elementContentNS (teiNS "corr")

gap :: NodeParser Gap
gap = build <$> Xml.elementAttrNS (teiNS "gap") (Xml.attribute "reason") Xml.end
  where
  build (x, _) = Gap x

contentText :: NodeParser Content
contentText = ContentText <$> Xml.content

content :: NodeParser Content
content
  = MP.try contentText
  <|> (ContentMilestone <$> milestone)
  <|> contentAdd
  <|> contentDel
  <|> (ContentGap <$> gap)
  <|> contentCorr

textPartSubtype :: Text -> Xml.AttributeParser Integer
textPartSubtype v = do
  n <- Xml.attribute "n"
  num <- Xml.parseNested (Text.unpack v ++ " number") MP.integer n
  _ <- Xml.attributeValue "subtype" v
  _ <- Xml.attributeValue "type" "textpart"
  return num

section :: NodeParser Section
section = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, y) = Section x y
  attributes = textPartSubtype "section"
  children = Xml.elementNS (teiNS "p") (many content)

chapter :: NodeParser Chapter
chapter = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, y) = Chapter x y
  attributes = textPartSubtype "chapter"
  children = many section

book :: NodeParser Book
book = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, (y, z)) = Book x y z
  attributes = textPartSubtype "book"
  children = do
    h <- Xml.elementContentNS (teiNS "head")
    cs <- many chapter
    return (h, cs)

division :: NodeParser Division
division
  = MP.try (DivisionBooks <$> many book)
  <|> (DivisionChapters <$> many chapter)

edition :: NodeParser Edition
edition = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build ((n, l), y) = Edition n l y
  attributes = do
    n <- Xml.attribute "n"
    _ <- Xml.attributeValue "type" "edition"
    l <- Xml.attributeXml "lang"
    return (n, l)
  children = division

body :: NodeParser Body
body = Xml.elementNS (teiNS "body") children
  where
  children = pure Body
    <*> edition

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
