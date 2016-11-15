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
    <*> funder

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

monogr :: NodeParser Monogr
monogr = Xml.elementNS (teiNS "monogr") children
  where
  children = foldMonogrProp <$> many
    (( MonogrPropAuthor <$> xmlContent "author")
    <|> (MonogrPropTitle <$> xmlContent "title")
    <|> (MonogrPropImprint <$> imprint)
    <|> (MonogrPropEditor <$> editor)
    )

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
correction = build <$> Xml.elementContentAttrNS (teiNS "correction") attributes
  where
  build (x, y) = Correction x y
  attributes = Xml.attribute "method"

editorialDecl :: NodeParser EditorialDecl
editorialDecl = EditorialDecl <$> Xml.elementNS (teiNS "editorialDecl") correction 

encodingDesc :: NodeParser EncodingDesc
encodingDesc = Xml.elementNS (teiNS "encodingDesc") children
  where
  children = pure EncodingDesc
    <*> (Maybe.maybe [] id <$> optional (MP.try refsDeclCts))
    <*> optional editorialDecl
    <*> refsDeclState

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
