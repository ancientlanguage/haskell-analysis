module Prepare.Perseus.TeiEpidocParserCommon where

import Data.Text (Text)
import Data.XML.Types
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml

teiNS :: Text -> Name
teiNS t = Name t (Just "http://www.tei-c.org/ns/1.0") Nothing

xmlContent :: Text -> NodeParser Text
xmlContent = Xml.elementContentNS . teiNS
