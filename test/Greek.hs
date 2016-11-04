module Greek where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, assertEqual)
import Control.Lens ((^?))
import Data.Either.Validation (Validation(..), _Failure)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Round
import Grammar.CommonTypes
import Grammar.Greek.Script.Types
import qualified Grammar.Greek.Script.Rounds as Rounds
import qualified Grammar.Greek.Script.Stage as Stage
import Grammar.Pretty
import Grammar.Prepare
import Grammar.Serialize
import Round

failMessage :: Text -> IO ()
failMessage = assertFailure . Text.unpack

testDataLoss
  :: [Milestone :* String]
  -> [Milestone :* String]
  -> IO ()
testDataLoss xs ys = check . filter (\(x, y) -> x /= y) $ zip xs ys
  where
  check [] = return ()
  check xs@(_ : _) = failMessage $ Text.concat
    [ "data loss:"
    , Text.concat $ fmap (\(x , y) -> Text.concat [ "\n initial:", prettyMilestonedString x, "\n final  :", prettyMilestonedString y ]) xs
    ]

testStage stg x = do
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
          where
          forget = Stage.forgetHasWordPunctuation

testSourceStage stg (SourceId g s, ms) = testCase (Text.unpack . Text.intercalate " " $ [g, s]) $ testStage stg ms

testGroupStages name stg = buildTestBracketed $ do
  result <- readGroups
  let
    sourceTests = case result of
      Left x -> [testCase "decode" $ assertFailure $ "decode failure:\n" ++ x]
      Right gs -> fmap (testSourceStage stg) (Stage.start gs)
  let sourceTestGroup = testGroup name sourceTests
  return (sourceTestGroup, return ())

unicodeSymbolTestGroup = testGroup "Unicode-Symbol" $ concat
  [ testList "unicodeSymbol letters" tr "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρσςτυφχψω"
  , testList "unicodeSymbol marks" tr "α\x0300\x0301\x0308\x0313\x0314\x0342\x0345\x2019"
  ]
  where
  tr = testRoundFwd Rounds.unicodeSymbol

vocalicSyllableTestGroup = testGroup "vocalic syllables" $
  [ test "Μωϋσῆς"
    [ (V_ω, Nothing)
    , (V_υ, Just S_Diaeresis)
    ]
  , test "διϋλίζοντες"
    [ (V_ι, Nothing)
    , (V_υ, Just S_Diaeresis)
    ]
  , test "πρωῒ"
    [ (V_ω, Nothing)
    , (V_ι, Just S_Diaeresis)
    ]
  , test "διϊσχυρίζετο"
    [ (V_ι, Nothing)
    , (V_ι, Just S_Diaeresis)
    ]
  , testDest "Ἁλληλουϊά"
    [ (V_ο, Nothing)
    , (V_υ, Nothing)
    , (V_ι, Just S_Diaeresis)
    , (V_α, Nothing)
    ]
    [ VS_Diphthong D_ου
    , VS_Vowel V_ι
    , VS_Vowel V_α
    ]
  , testDest "Δαυίδ"
    [ (V_α, Nothing)
    , (V_υ, Nothing)
    , (V_ι, Nothing)
    ]
    [ VS_Vowel V_α
    , VS_Diphthong D_υι
    ]
  , testDest "εὐποιΐας"
    [ (V_ο, Nothing)
    , (V_ι, Nothing)
    , (V_ι, Just S_Diaeresis)
    , (V_α, Nothing)
    ]
    [ VS_Diphthong D_οι
    , VS_Vowel V_ι
    , VS_Vowel V_α
    ]
  ]
  where
  test n vs = testRoundId (Rounds.vocalicSyllable ()) n $ mapUnit3 vs
  mapUnit3 = fmap (\(x, y) -> (x, (y, ())))
  mapUnit2 = fmap (\x -> (x, ()))
  testDest n vs ds = testRoundIdDest (Rounds.vocalicSyllable ()) n (mapUnit3 vs) (mapUnit2 ds)

finalTestGroup = testGroup "final forms" $
  [ testRoundFwd Rounds.final "medial and final sigma"
    [ ((L_σ, NotFinal), ())
    , ((L_σ, IsFinal), ())
    ]
  , testRoundFwd Rounds.final "medial sigma, final alpha"
    [ ((L_σ, NotFinal), ())
    , ((L_α, IsFinal), ())
    ]
  , nonFinalSigmaFailure
  ]
  where
  nonFinalSigmaFailure = testCase "non-final sigma failure" $ do
    let
      input =
        [ ((L_α, NotFinal), ())
        , ((L_σ, NotFinal), ())
        ]
    let x = (roundFwdTo Rounds.final) input
    assertEqual "expect fail on non-final sigma in final position" True (isJust $ x ^? _Failure)

greekGroups =
  [ unicodeSymbolTestGroup
  , vocalicSyllableTestGroup
  , finalTestGroup
  , testGroupStages "script stage" Stage.script
  ]
