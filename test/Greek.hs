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

prettyMilestoned :: Show a => Milestone :* a -> String
prettyMilestoned (m, x) = Text.unpack . Text.intercalate " " $
  [ prettyMilestone m
  , "--"
  , textShow $ x
  ]

testDataLoss
  :: (Eq a, Show a)
  => [Milestone :* a]
  -> [Milestone :* a]
  -> IO ()
testDataLoss xs ys = mapM_ check $ zip xs ys
  where
  check :: (Eq a, Show a) => ((Milestone, a), (Milestone, a)) -> IO ()
  check (x , y) = case x == y of
    True -> return ()
    False -> assertFailure $ "data loss:\n" ++ prettyMilestoned x ++ "\n" ++ prettyMilestoned y

testStages x = do
  let stageTo = aroundTo $ stageAround stage
  let stageFrom = aroundFrom $ stageAround stage
  case stageTo x of
    Failure es -> assertFailure $ "stage to failure:" ++ concatMap (('\n' :) . prettyMilestoned) es
    Success y ->
      case stageFrom y of
        Failure es' -> assertFailure $ "stage from failure:" ++ concatMap (('\n' :) . prettyMilestoned) es'
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
    [ testAround "unicodeSymbol letters" unicodeSymbol "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
    , testAround "unicodeSymbol marks" unicodeSymbol "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
    ]
  , testGroup "Stages" [ testGroupStages ]
  ]
