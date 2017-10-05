module Grammar.Greek.Script.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize
import Primary

decodeGroups :: BS.ByteString -> Either String [Group]
decodeGroups = Serialize.decode

groupsPath :: FilePath
groupsPath = "../modules/binary-primary/data/groups.data"

readGroups :: IO (Either String [Group])
readGroups = decodeGroups <$> BS.readFile groupsPath
