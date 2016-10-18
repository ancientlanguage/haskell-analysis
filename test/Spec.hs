import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Grammar.Serialize

testDecodeGroups :: Test
testDecodeGroups = testCase "groups" $ do
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right x -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Decode" [ testDecodeGroups ]
  ]
