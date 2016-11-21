module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
import Control.Lens (over, _Just)
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Perseus.TeiEpidocModel
import Prepare.Perseus.TeiEpidocHeaderParser
import Prepare.Perseus.TeiEpidocParserCommon
import Prepare.Xml.Parser (NodeParser, (<|>), many, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP

milestoneParagraph :: NodeParser Milestone
milestoneParagraph = build <$> Xml.elementAttrNS (teiNS "milestone") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    ed <- Xml.attribute "ed"
    u <- Xml.attribute "unit"
    _ <- Xml.parseNested ("milestone unit para") (MP.string "para") u
    return $ MilestoneParagraph ed

milestoneCard :: NodeParser Milestone
milestoneCard = build <$> Xml.elementAttrNS (teiNS "milestone") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    n <- Xml.attribute "n"
    num <- Xml.parseNested "milestone card n" MP.integer n
    u <- Xml.attribute "unit"
    _ <- Xml.parseNested "milestone unit card" (MP.string "card") u
    return $ MilestoneCard num

milestone :: NodeParser Milestone
milestone
  = MP.try milestoneParagraph
  <|> milestoneCard

apparatusAdd :: NodeParser ApparatusAdd
apparatusAdd = ApparatusAdd <$> Xml.elementContentNS (teiNS "add")

apparatusDel :: NodeParser ApparatusDel
apparatusDel = ApparatusDel <$> Xml.elementContentNS (teiNS "del")

apparatusCorr :: NodeParser ApparatusCorr
apparatusCorr = ApparatusCorr <$> Xml.elementContentNS (teiNS "corr")

term :: NodeParser Term
term = Term <$> Xml.elementContentNS (teiNS "term")

gap :: NodeParser Gap
gap = build <$> Xml.elementAttrNS (teiNS "gap") (optional $ Xml.attribute "reason") Xml.end
  where
  build (x, _) = Gap x

plainText :: NodeParser Text
plainText = Xml.content

bibl :: NodeParser Bibl
bibl = build <$> Xml.elementContentAttrNS (teiNS "bibl") attributes
  where
  build (x, t) = Bibl x t
  attributes = optional (Xml.attribute "n")

quoteLine :: NodeParser QuoteLine
quoteLine = build <$> Xml.elementContentAttrNS (teiNS "l") attributes
  where
  build (x, y) = QuoteLine x y
  attributes = optional (Xml.attribute "met")

quote :: NodeParser Quote
quote = build <$> Xml.elementAttrNS (teiNS "quote") attributes children
  where
  build (x, y) = Quote x y
  attributes = Xml.attribute "type"
  children = many quoteLine

cit :: NodeParser Cit
cit = Xml.elementNS (teiNS "cit") (Cit <$> quote <*> bibl)

content :: NodeParser Content
content
  = MP.try (ContentText <$> plainText)
  <|> (ContentAdd <$> apparatusAdd)
  <|> (ContentDel <$> apparatusDel)
  <|> (ContentCorr <$> apparatusCorr)
  <|> (ContentTerm <$> term)
  <|> (ContentMilestone <$> milestone)
  <|> (ContentGap <$> gap)
  <|> (ContentQuote <$> quote)
  <|> (ContentBibl <$> bibl)
  <|> (ContentCit <$> cit)

textPartSubtype :: Text -> Xml.AttributeParser Integer
textPartSubtype v = do
  n <- Xml.attribute "n"
  num <- Xml.parseNested (Text.unpack v ++ " number") MP.integer n
  _ <- Xml.attributeValue "subtype" v
  _ <- Xml.attributeValue "type" "textpart"
  return num

divType :: Text -> Xml.AttributeParser Integer
divType v = do
  n <- Xml.attribute "n"
  num <- Xml.parseNested (Text.unpack v ++ " number") MP.integer n
  _ <- Xml.attributeValue "type" v
  return num

divTypeOrSubtype :: Text -> Xml.AttributeParser Integer
divTypeOrSubtype v
  = MP.try (textPartSubtype v)
  <|> divType v

section :: NodeParser Section
section = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, y) = Section x y
  attributes = divTypeOrSubtype "section"
  children = concat <$> many (Xml.elementNS (teiNS "p") (many content))

chapter :: NodeParser Chapter
chapter = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, y) = Chapter x y
  attributes = divTypeOrSubtype "chapter"
  children = many section

book :: NodeParser Book
book = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, (y, z)) = Book x y z
  attributes = divTypeOrSubtype "book"
  children = do
    h <- Xml.elementContentNS (teiNS "head")
    cs <- many chapter
    return (h, cs)

lineContent :: NodeParser LineContent
lineContent
  = MP.try (LineContentMilestone <$> milestone)
  <|> MP.try (LineContentText <$> plainText)
  <|> (LineContentDel <$> apparatusDel)

line :: NodeParser Line
line = build <$> Xml.elementAttrNS (teiNS "l") attributes children
  where
  build ((n, r), cs) = Line n r cs
  attributes = do
    n <- optional (Xml.attribute "n")
    num <- _Just (Xml.parseNested "line number" MP.integer) n
    rend <- optional (Xml.attribute "rend")
    r <- _Just (Xml.parseNested "line rend" $ MP.string "displayNumAndIndent") rend
    return (num, over _Just (const LineRender_DisplayNumAndIndent) r)
  children = many lineContent

bookLineContent :: NodeParser BookLineContent
bookLineContent
  = MP.try (BookLineContentMilestone <$> milestone)
  <|> (BookLineContentLine <$> line)

bookLines :: NodeParser BookLines
bookLines = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build (x, y) = BookLines x y
  attributes = divTypeOrSubtype "book"
  children = many bookLineContent

division :: NodeParser Division
division
  = MP.try (DivisionBooks <$> many book)
  <|> MP.try (DivisionChapters <$> many chapter)
  <|> MP.try (DivisionSections <$> many section)
  <|> (DivisionBookLines <$> many bookLines)

edition :: NodeParser Edition
edition = build <$> Xml.elementAttrNS (teiNS "div") attributes children
  where
  build ((n, l), y) = Edition n l y
  attributes = do
    n <- Xml.attribute "n"
    _ <- Xml.attributeValue "type" "edition"
    l <- optional (Xml.attributeXml "lang")
    return (n, l)
  children = division

body :: NodeParser Body
body = Xml.elementNS (teiNS "body") children
  where
  children
    = MP.try (BodyEdition <$> edition)
    <|> (BodyDivision <$> division)

teiText :: NodeParser TeiText
teiText = build <$> Xml.elementAttrNS (teiNS "text") attributes children
  where
  build (l, b) = TeiText l b
  attributes = optional (Xml.attributeXml "lang")
  children = body

tei :: NodeParser Tei
tei = Xml.elementNS (teiNS "TEI") children 
  where
  children = pure Tei
    <*> teiHeader
    <*> teiText
