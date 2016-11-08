import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, assertEqual)
import Control.Lens (over, _2, (^?), _Right)
import Data.Either.Validation (Validation(..), _Failure)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Common.Prepare
import Grammar.Common.Pretty
import Grammar.Common.Round
import Grammar.Common.Serialize
import Grammar.Common.Types
import Grammar.Greek.Script.Types
import qualified Grammar.Greek.Script.Rounds as Rounds
import qualified Grammar.Greek.Script.Serialize as Serialize
import qualified Grammar.Greek.Script.Stage as Stage
import Grammar.Test.Round
import Grammar.Test.Stage

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
  , testGroupStages "script stage" Stage.script Stage.forgetHasWordPunctuation (fmap (over _Right Stage.start) $ Serialize.readGroups)
  ]

main :: IO ()
main = defaultMain greekGroups
