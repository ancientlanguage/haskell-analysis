module Prepare.Perseus.TeiEpidocParser where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Sblgnt.Model
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP
import Data.XML.Types

teiNS :: Text -> Name
teiNS t = Name t (Just "http://www.tei-c.org/ns/1.0") Nothing

xmlNS :: Text -> Name
xmlNS t = Name t (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

tei :: NodeParser (Text, ())
tei = Xml.elementAttrNS (teiNS "TEI") attributes children 
  where
  attributes = return ""
  children = do
    return $ ()
