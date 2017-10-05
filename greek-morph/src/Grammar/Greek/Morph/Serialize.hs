module Grammar.Greek.Morph.Serialize where

import Prelude hiding (Word)
import Grammar.Common.Types
import Grammar.IO.Serialize
import Grammar.Greek.Script.Word
import System.FilePath ((</>))

scriptPath :: FilePath -> FilePath
scriptPath modulesPath = modulesPath </> "binary-greek-script/data"

readScript :: FilePath -> IO [SourceId :* [Milestone :* Word]]
readScript modulesPath = loadStage $ scriptPath modulesPath
