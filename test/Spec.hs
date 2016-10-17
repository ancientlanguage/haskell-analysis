import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import qualified Data.ByteString as BS
import qualified Data.Serialize as Serialize
import Primary
import Lib

decodeGroups :: BS.ByteString -> Either String [Group]
decodeGroups = Serialize.decode

testDecodeGroups :: Test
testDecodeGroups = testCase "groups" $ do
  let path = "./modules/binary-primary/data/groups.data"
  encoded <- BS.readFile path
  case decodeGroups encoded of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right x -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Decode" [ testDecodeGroups ]
  ]
