module Greek where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Around
import Grammar.CommonTypes
import qualified Grammar.Greek.Script.Around as Around
import Grammar.Greek.Stage
import Grammar.Pretty
import Grammar.Prepare
import Grammar.Serialize
import Around

failMessage :: Text -> IO ()
failMessage = assertFailure . Text.unpack

testDataLoss
  :: (Eq a, Show a)
  => [Milestone :* a]
  -> [Milestone :* a]
  -> IO ()
testDataLoss xs ys = check . filter (\(x, y) -> x /= y) $ zip xs ys
  where
  check [] = return ()
  check xs@(_ : _) = failMessage $ Text.concat
    [ "data loss:"
    , Text.concat $ fmap (\(x , y) -> Text.concat [ "\n initial:", prettyMilestoned x, "\n final  :", prettyMilestoned y ]) xs
    ]

testStages x = do
  let stageTo = aroundTo $ stageAround stage
  let stageFrom = aroundFrom $ stageAround stage
  case stageTo x of
    Failure es -> failMessage $ Text.concat
      [ "stage to failure:"
      , Text.concat $ fmap (Text.append "\n" . prettyMilestoned) es
      ]
    Success y ->
      case stageFrom y of
        Failure es' -> failMessage $ Text.concat
          [ "stage from failure:"
          , Text.concat $ fmap (Text.append "\n" . prettyMilestoned) es'
          ]
        Success z -> testDataLoss ((stageForget stage) x) ((stageForget stage) z)

testSourceStages (SourceId g s, ms) = do
  _ <- Text.putStrLn $ Text.intercalate " " [g, s]
  testStages ms

testGroupStages :: Test
testGroupStages = testCase "around stages" $ do
  result <- readGroups
  case result of
    Left x -> assertFailure $ "decode failure:\n" ++ x
    Right gs -> mapM_ testSourceStages (start gs)

greekGroups =
  [ testGroup "Unicode-Symbol" $ concat
    [ testAround "unicodeSymbol letters" Around.unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
    , testAround "unicodeSymbol marks" Around.unicodeSymbol "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
    ]
  , testGroup "Stages" [ testGroupStages ]
  ]
