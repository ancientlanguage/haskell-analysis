module Greek where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as Text
import Grammar.CommonTypes
import Grammar.Prepare
import Grammar.Greek.Script.UnicodeSymbol
import Grammar.Greek.Stage
import Grammar.Serialize
import qualified Primary
import Around

textShow :: Show a => a -> Text
textShow = Text.pack . show

prettyMilestone :: Maybe Primary.Verse :* Maybe Paragraph -> Text
prettyMilestone (Nothing, _) = ""
prettyMilestone (Just (Primary.Verse c v), _) = Text.concat [textShow c, ":", textShow v]

prettySource :: Show a => SourceId :* Milestone :* a -> String
prettySource (SourceId g s, (m, x)) = Text.unpack . Text.intercalate " " $
  [ g
  , s
  , prettyMilestone m
  , "--"
  , textShow $ x
  ]

testStages :: Test
testStages = testCase "around stages" $ do
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right gs ->
      case stage0 (start gs) of
        Failure es -> assertFailure $ "stage failure:" ++ concatMap (('\n' :) . prettySource) es
        Success _ -> return ()

greekGroups =
  [ testGroup "Unicode-Symbol" $ concat
    [ testAround "unicodeSymbol letters" unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
    , testAround "unicodeSymbol marks" unicodeSymbol "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
    ]
  , testGroup "Stages" [ testStages ]
  ]
