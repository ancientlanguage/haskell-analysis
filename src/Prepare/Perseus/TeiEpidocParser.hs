module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
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
  children = do
    r <- xmlContent "resp"
    ns <- many (xmlContent "name")
    return $ RespStmt r ns

titleStmt :: NodeParser TitleStmt
titleStmt = Xml.elementNS (teiNS "titleStmt") children
  where
  children = do
    t <- xmlContent "title"
    a <- xmlContent "author"
    s <- xmlContent "sponsor"
    p <- xmlContent "principal"
    r <- respStmt
    f <- funder
    return $ TitleStmt t a s p r f

fileDesc :: NodeParser FileDesc
fileDesc = FileDesc <$> Xml.elementNS (teiNS "fileDesc") titleStmt

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
