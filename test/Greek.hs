module Greek where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Grammar.Greek.Script.UnicodeSymbol
import Grammar.Greek.Stage
import Grammar.Serialize
import Around

testStages :: Test
testStages = testCase "around stages" $ do
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right gs ->
      case stage0 (start gs) of
        Failure es -> assertFailure $ "stage failure:" ++ concatMap (('\n' :) . show) es
        Success _ -> return ()

greekGroups =
  [ testGroup "Unicode-Symbol" $ concat
    [ testAround "unicodeSymbol letters" unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
    , testAround "unicodeSymbol marks" unicodeSymbol "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
    ]
  , testGroup "Stages" [ testStages ]
  ]
