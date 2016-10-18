module Grammar.Greek.Script.Stage where

import Prelude hiding (Word)
import Data.Text (Text)
import Primary
import Grammar.CommonTypes

makeInitialSources :: [Group] -> [SourceId :* ]