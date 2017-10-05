module Grammar.Greek.Morph.Serialize where

import Prelude hiding (Word)
import Grammar.Common.Types
import Grammar.IO.Serialize
import Grammar.Greek.Script.Word

scriptPath :: FilePath
scriptPath = "./modules/binary-greek-script/data"

readScript :: IO [SourceId :* [Milestone :* Word]]
readScript = loadStage scriptPath
