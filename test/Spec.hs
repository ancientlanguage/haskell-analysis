import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Grammar.Around
import Grammar.Greek.Script.UnicodeSymbol
import Grammar.Serialize

testDecodeGroups :: Test
testDecodeGroups = testCase "groups" $ do
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right x -> return ()

testAround
  :: (Show a, Show e1, Show e2)
  => Around e1 e2 a b
  -> [a]
  -> [Test]
testAround (Around f g) = fmap testToFrom
  where
  testToFrom x = testCase ("to/from " ++ show x) $ do
    case f x of
      Left e1 -> assertFailure $ "to failure: " ++ show e1
      Right y ->
        case g y of
          Left e2 -> assertFailure $ "from failure: " ++ show e2
          Right _ -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Decode" [ testDecodeGroups ]
  , testGroup "Unicode-Symbol" $ testAround unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψωα\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
  ]
