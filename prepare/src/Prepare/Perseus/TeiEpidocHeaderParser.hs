module Prepare.Perseus.TeiEpidocHeaderParser where

import Prelude hiding (Word)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
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
    <*> optional funder

imprint :: NodeParser Imprint
imprint = Xml.elementNS (teiNS "imprint") children
  where
  children = do
    pp <- optional $ xmlContent "pubPlace"
    p <- xmlContent "publisher"
    d <- xmlContent "date"
    return $ Imprint p d pp

editor :: NodeParser Editor
editor = build <$> Xml.elementContentAttrNS (teiNS "editor") attributes
  where
  build (x, y) = Editor x y
  attributes = Xml.attribute "role"

data MonogrProp
  = MonogrPropAuthor Text
  | MonogrPropTitle Text
  | MonogrPropImprint Imprint
  | MonogrPropEditor Editor

foldMonogrProp :: [MonogrProp] -> Monogr
foldMonogrProp = foldr go (Monogr Nothing Nothing Nothing Nothing)
  where
  go (MonogrPropAuthor x) m = m { monogrAuthor = Just x }
  go (MonogrPropTitle x) m = m { monogrTitle = Just x }
  go (MonogrPropImprint x) m = m { monogrImprint = Just x }
  go (MonogrPropEditor x) m = m { monogrEditor = Just x }

monogrChildren :: NodeParser Monogr
monogrChildren = foldMonogrProp <$> many
  (( MonogrPropAuthor <$> xmlContent "author")
  <|> (MonogrPropTitle <$> xmlContent "title")
  <|> (MonogrPropImprint <$> imprint)
  <|> (MonogrPropEditor <$> editor)
  )

monogr :: NodeParser Monogr
monogr = Xml.elementNS (teiNS "monogr") monogrChildren

biblStruct :: NodeParser BiblStruct
biblStruct = build <$> Xml.elementAttrNS (teiNS "biblStruct") attributes monogr
  where
  build (a, b) = BiblStruct a b
  attributes = optional (Xml.attribute "default")

bibl :: NodeParser BiblStruct
bibl = build <$> Xml.elementAttrNS (teiNS "bibl") attributes children
  where
  build (a, m) = BiblStruct a m
  attributes = optional (Xml.attribute "default")
  children = do
    a <- xmlContent "author"
    t <- xmlContent "title"
    p <- xmlContent "publisher"
    d <- xmlContent "date"
    return $ Monogr (Just a) (Just t) (Just (Imprint p d Nothing)) Nothing

sourceDesc :: NodeParser SourceDesc
sourceDesc = build <$> Xml.elementAttrNS (teiNS "sourceDesc") attributes children
  where
  build (a, b) = SourceDesc a b
  attributes = optional (Xml.attribute "default")
  children
    = MP.try biblStruct
    <|> bibl

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
    <*> optional (xmlContent "extent")
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

refsDeclCts :: NodeParser [CRefPattern]
refsDeclCts = build <$> Xml.elementAttrNS (teiNS "refsDecl") attributes children
  where
  build (_, x) = x
  attributes = Xml.attributeValue "n" "CTS"
  children = many cRefPattern

refState :: NodeParser RefState
refState = build <$> Xml.elementAttrNS (teiNS "refState") attributes Xml.end
  where
  build (x, _) = x
  attributes = do
    d <- optional (Xml.attribute "delim")
    u <- Xml.attribute "unit"
    return $ RefState u d

refsDeclState :: NodeParser [RefState]
refsDeclState = Xml.elementNS (teiNS "refsDecl") (many refState)

correction :: NodeParser Correction
correction = build <$> Xml.elementAttrNS (teiNS "correction") attributes children
  where
  build (x, y) = Correction x y
  attributes = Xml.attribute "method"
  children = Xml.elementContentNS (teiNS "p")

editorialDecl :: NodeParser EditorialDecl
editorialDecl = EditorialDecl <$> Xml.elementNS (teiNS "editorialDecl") correction

encodingDesc :: NodeParser EncodingDesc
encodingDesc = Xml.elementNS (teiNS "encodingDesc") children
  where
  children = pure EncodingDesc
    <*> (Maybe.maybe [] id <$> optional (MP.try refsDeclCts))
    <*> optional editorialDecl
    <*> (Maybe.maybe [] id <$> optional refsDeclState)

language :: NodeParser Language
language = build <$> Xml.elementContentAttrNS (teiNS "language") attributes
  where
  build ((i, u), c) = Language i u c
  attributes = do
    i <- Xml.attribute "ident"
    u <- optional (Xml.attribute "usage")
    return (i, u)

langUsage :: NodeParser LangUsage
langUsage = build <$> Xml.elementAttrNS (teiNS "langUsage") attributes (many language)
  where
  build (a, b) = LangUsage a b
  attributes = optional (Xml.attribute "default")

profileDesc :: NodeParser ProfileDesc
profileDesc = ProfileDesc <$> Xml.elementNS (teiNS "profileDesc") langUsage

change :: NodeParser Change
change = build <$> Xml.elementAttrNS (teiNS "change") attributes children
  where
  build ((x, y), z) = Change x y z
  attributes = pure (,)
    <*> Xml.attribute "when"
    <*> Xml.attribute "who"
  children
    =   fmap Just Xml.onlyContent
    <|> fmap (const Nothing) Xml.end

revisionDesc :: NodeParser RevisionDesc
revisionDesc = RevisionDesc <$> Xml.elementNS (teiNS "revisionDesc") (many change)

teiHeader :: NodeParser TeiHeader
teiHeader = build <$> Xml.elementNS (teiNS "teiHeader") children
  where
  build (fd, ed, pd, rd) = TeiHeader fd ed pd rd
  children = pure (,,,)
    <*> fileDesc
    <*> encodingDesc
    <*> profileDesc
    <*> revisionDesc
