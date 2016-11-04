module Grammar.Test.Stage where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure)
import Data.Either.Validation (Validation(..))
import Grammar.CommonTypes
import Grammar.Prepare
import Grammar.Pretty
import Grammar.Round

failMessage :: Text -> IO ()
failMessage = assertFailure . Text.unpack

testDataLoss
  :: [Milestone :* String]
  -> [Milestone :* String]
  -> IO ()
testDataLoss xs ys = check . filter (\(x, y) -> x /= y) $ zip xs ys
  where
  check [] = return ()
  check xs'@(_ : _) = failMessage $ Text.concat
    [ "data loss:"
    , Text.concat $ fmap (\(x , y) -> Text.concat [ "\n initial:", prettyMilestonedString x, "\n final  :", prettyMilestonedString y ]) xs'
    ]

testStage
  :: (Show a1, Show a)
  => Round (Milestone :* a) (Milestone :* a1) t b
  -> (t -> [Milestone :* String])
  -> t
  -> IO ()
testStage stg forget x = do
  let stageTo = roundTo stg
  let stageFrom = roundFrom stg
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
        Success z -> testDataLoss (forget x) (forget z)

testSourceStage
  :: (Show a1, Show a)
  => Round (Milestone :* a) (Milestone :* a1) t b
  -> (t -> [Milestone :* String])
  -> (SourceId, t)
  -> Test
testSourceStage stg forget (SourceId g s, ms) = testCase (Text.unpack . Text.intercalate " " $ [g, s]) $ testStage stg forget ms

testGroupStages
  :: (Show a1, Show a)
  => TestName
  -> Round (Milestone :* a) (Milestone :* a1) t b
  -> (t -> [Milestone :* String])
  -> IO (Either [Char] [(SourceId, t)])
  -> Test
testGroupStages name stg forget load = buildTestBracketed $ do
  result <- load
  let
    sourceTests = case result of
      Left x -> [testCase "decode" $ assertFailure $ "decode failure:\n" ++ x]
      Right gs -> fmap (testSourceStage stg forget) gs
  let sourceTestGroup = testGroup name sourceTests
  return (sourceTestGroup, return ())
