module Prepare.Perseus.TeiEpidocHeaderParser where

import Prelude hiding (Word)
import Prepare.Perseus.TeiEpidocHeaderModel
import Prepare.Perseus.TeiEpidocParserCommon
import Prepare.Xml.Parser (NodeParser, (<|>), many, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Prim as MP

funder :: NodeParser Funder
funder = build <$> Xml.elementContentAttrNS (teiNS "funder") attributes
  where
  build = uncurry Funder
  attributes = Xml.attribute "n"

respStmt :: NodeParser RespStmt
respStmt = Xml.elementNS (teiNS "respStmt") children
  where
  children = pure RespStmt
    <*> xmlContent "resp"
    <*> many (xmlContent "name")

titleStmt :: NodeParser TitleStmt
titleStmt = Xml.elementNS (teiNS "titleStmt") children
  where
  children = pure TitleStmt
    <*> xmlContent "title"
    <*> xmlContent "author"
    <*> xmlContent "sponsor"
    <*> xmlContent "principal"
    <*> respStmt
    <*> funder

imprint :: NodeParser Imprint
imprint = Xml.elementNS (teiNS "imprint") children
  where
  children = pure Imprint
    <*> xmlContent "publisher"
    <*> xmlContent "date"

monogr :: NodeParser Monogr
monogr = Xml.elementNS (teiNS "monogr") children
  where
  children = pure Monogr
    <*> xmlContent "author"
    <*> xmlContent "title"
    <*> imprint

biblStruct :: NodeParser BiblStruct
biblStruct = BiblStruct <$> Xml.elementNS (teiNS "biblStruct") monogr

sourceDesc :: NodeParser SourceDesc
sourceDesc = SourceDesc <$> Xml.elementNS (teiNS "sourceDesc") biblStruct

publicationStmt :: NodeParser PublicationStmt
publicationStmt = Xml.elementNS (teiNS "publicationStmt") children
  where
  children = pure PublicationStmt
    <*> xmlContent "publisher"
    <*> xmlContent "pubPlace"
    <*> xmlContent "authority"

fileDesc :: NodeParser FileDesc
fileDesc = Xml.elementNS (teiNS "fileDesc") children
  where
  children = pure FileDesc
    <*> titleStmt
    <*> xmlContent "extent"
    <*> publicationStmt
    <*> sourceDesc

cRefPattern :: NodeParser CRefPattern
cRefPattern = build <$> Xml.elementAttrNS (teiNS "cRefPattern") attributes children
  where
  build ((n, mp, rp), p) = CRefPattern n mp rp p
  attributes = do
    mp <- Xml.attribute "matchPattern"
    n <- Xml.attribute "n"
    rp <- Xml.attribute "replacementPattern"
    return (mp, n, rp)
  children = Xml.elementContentNS (teiNS "p")

refsDeclCts :: NodeParser RefsDecl
refsDeclCts = build <$> Xml.elementAttrNS (teiNS "refsDecl") attributes children
  where
  build (_, x) = x
  attributes = Xml.attributeValue "n" "CTS"
  children = RefsDeclCts <$> many cRefPattern

refState :: NodeParser RefState
refState = build <$> Xml.elementAttrNS (teiNS "refState") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    d <- optional (Xml.attribute "delim")
    u <- Xml.attribute "unit"
    return $ RefState u d

refsDeclState :: NodeParser RefsDecl
refsDeclState = RefsDeclState <$> Xml.elementNS (teiNS "refsDecl") (many refState)

refsDecl :: NodeParser RefsDecl
refsDecl
  = MP.try refsDeclCts
  <|> refsDeclState

encodingDesc :: NodeParser EncodingDesc
encodingDesc = Xml.elementNS (teiNS "encodingDesc") children
  where
  children = pure EncodingDesc
    <*> many refsDecl

language :: NodeParser Language
language = build <$> Xml.elementContentAttrNS (teiNS "language") attributes
  where
  build (i, c) = Language i c
  attributes = Xml.attribute "ident"

langUsage :: NodeParser LangUsage
langUsage = LangUsage <$> Xml.elementNS (teiNS "langUsage") (many language)

profileDesc :: NodeParser ProfileDesc
profileDesc = ProfileDesc <$> Xml.elementNS (teiNS "profileDesc") langUsage

change :: NodeParser Change
change = build <$> Xml.elementContentAttrNS (teiNS "change") attributes
  where
  build ((x, y), z) = Change x y z
  attributes = pure (,)
    <*> Xml.attribute "when"
    <*> Xml.attribute "who"

revisionDesc :: NodeParser RevisionDesc
revisionDesc = RevisionDesc <$> Xml.elementNS (teiNS "revisionDesc") (many change)

teiHeader :: NodeParser TeiHeader
teiHeader = build <$> Xml.elementAttrNS (teiNS "teiHeader") attributes children
  where
  build (t, (fd, ed, pd, rd)) = TeiHeader t fd ed pd rd
  attributes = do
    t <- Xml.attribute "type"
    return t
  children = pure (,,,)
    <*> fileDesc
    <*> encodingDesc
    <*> profileDesc
    <*> revisionDesc
