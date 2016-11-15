module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types
import Prepare.Perseus.TeiEpidocModel
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP

teiNS :: Text -> Name
teiNS t = Name t (Just "http://www.tei-c.org/ns/1.0") Nothing

xmlContent :: Text -> NodeParser Text
xmlContent = Xml.elementContentNS . teiNS

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
    n <- Xml.attribute "n"
    mp <- Xml.attribute "matchPattern"
    rp <- Xml.attribute "replacementPattern"
    return (n, mp, rp)
  children = Xml.elementContentNS (teiNS "p")

refsDeclCts :: NodeParser RefsDecl
refsDeclCts = build <$> Xml.elementAttrNS (teiNS "refsDecl") attributes children
  where
  build (_, x) = x
  attributes = do
    n <- Xml.attribute "n"
    guard (n == "CTS")
    return ()
  children = RefsDeclCts <$> many cRefPattern

refsDecl :: NodeParser RefsDecl
refsDecl = refsDeclCts -- <|> refsDeclState

encodingDesc :: NodeParser EncodingDesc
encodingDesc = Xml.elementNS (teiNS "encodingDesc") children
  where
  children = pure EncodingDesc
    <*> many refsDecl

teiHeader :: NodeParser TeiHeader
teiHeader = build <$> Xml.elementAttrNS (teiNS "teiHeader") attributes children
  where
  build (t, fd) = TeiHeader t fd
  attributes = do
    t <- Xml.attribute "type"
    return t
  children = fileDesc

tei :: NodeParser Tei
tei = Xml.elementNS (teiNS "TEI") children 
  where
  children = do
    h <- teiHeader
    return $ Tei h
