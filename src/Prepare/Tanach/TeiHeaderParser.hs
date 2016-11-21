module Prepare.Tanach.TeiHeaderParser where

import Prelude hiding (Word)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Prepare.Xml.Parser (NodeParser, (<|>), many, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Prim as MP

