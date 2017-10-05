module Grammar.Greek.Script.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize
import System.FilePath ((</>))
import Primary

decodeGroups :: BS.ByteString -> Either String [Group]
decodeGroups = Serialize.decode

groupsPath :: FilePath -> FilePath
groupsPath modulesPath = modulesPath </> "binary-primary/data/groups.data"

readGroups :: FilePath -> IO (Either String [Group])
readGroups modulesPath  = decodeGroups <$> BS.readFile (groupsPath modulesPath)
