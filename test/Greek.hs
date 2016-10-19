module Greek where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as Text
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Prepare
import Grammar.Greek.Script.UnicodeSymbol
import Grammar.Greek.Stages
import Grammar.Serialize
import Around

textShow :: Show a => a -> Text
textShow = Text.pack . show

prettyMilestone :: Maybe Verse :* Maybe Paragraph -> Text
prettyMilestone (Nothing, _) = ""
prettyMilestone (Just (Verse c v), _) = Text.concat [textShow c, ":", textShow v]

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
  let stage = stage0
  let stageTo = aroundTo $ stageAround stage
  let stageFrom = aroundFrom $ stageAround stage
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right gs ->
      case stageTo (start gs) of
        Failure es -> assertFailure $ "stage to failure:" ++ concatMap (('\n' :) . prettySource) es
        Success y ->
          case stageFrom y of
            Failure es' -> assertFailure $ "stage from failure:" ++ concatMap (('\n' :) . prettySource) es'
            Success z ->
              case (stageForget stage) (start gs) == (stageForget stage) z of
                False -> assertFailure $ "data loss"
                True -> return ()

greekGroups =
  [ testGroup "Unicode-Symbol" $ concat
    [ testAround "unicodeSymbol letters" unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
    , testAround "unicodeSymbol marks" unicodeSymbol "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
    ]
  , testGroup "Stages" [ testStages ]
  ]
