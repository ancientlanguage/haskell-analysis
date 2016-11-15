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

tei :: NodeParser Tei
tei = Xml.elementNS (teiNS "TEI") children 
  where
  children = do
    h <- teiHeader
    return $ Tei h
