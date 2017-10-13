import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure, assertEqual, Assertion)
import Control.Lens (over, _2, (^?), _Right)
import Data.Either.Validation (Validation(..), _Failure)
import Data.Maybe (isJust)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Grammar.Common.Prepare
import Grammar.Common.Pretty
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.IO.Serialize
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import qualified Grammar.Greek.Morph.ShouldElide.Round as Round
import qualified Grammar.Greek.Morph.Serialize as Serialize
import qualified Grammar.Greek.Morph.Stage as Stage
import Grammar.Greek.Morph.Forms.Eliding (elidingForms)
import Grammar.Greek.Morph.Forms.Enclitic (encliticForms)
import Grammar.Greek.Morph.Forms.Proclitic (procliticForms)
import Grammar.Greek.Morph.Paradigm.Ending (allSplitsList, longestPrefix)
import Grammar.Test.Round
import Grammar.Test.Stage
import WordTests

allSplitsListGroup = testGroup "allSplitsList" $
  [ testCase "empty" $ assertEqual "empty"
    (allSplitsList ([] :: [()]))
    [[] :^ []]
  , testCase "123" $ assertEqual "123"
    (allSplitsList [1,2,3])
    [[] :^ [1,2,3], [1] :^ [2,3], [1,2] :^ [3], [1,2,3] :^ []]
  ]

longestPrefixGroup = testGroup "longestPrefix" $
  [ testCase "empty" $ assertEqual "empty" ([] :: [()]) (longestPrefix [])
  , testCase "123" $ assertEqual "123" [1,2,3] (longestPrefix [[1,2,3], [1,2,3,4,5], [1,2,3,4]])
  ]

isUnique :: (Show a, Eq a, Ord a) => [a] -> Assertion
isUnique xs = mapM_ (\(x, y) -> assertEqual "equal" x y) $ zip (List.sort xs) (Set.toList . Set.fromList $ xs)

uniqueFormsGroup = testGroup "unique forms" $
  [ testCase "unique enclitic" (isUnique encliticForms)
  , testCase "unique proclitic" (isUnique procliticForms)
  , testCase "unique eliding" (isUnique elidingForms)
  ]

shouldElideGroup = testGroup "shouldElide" $
  [ testDest "δ’"
    (CoreWord NoInitialAspiration [] [CR_δ] :^ IsElided :^ NoInitialAspiration)
    (CoreWord NoInitialAspiration [Syllable [CR_δ] (VS_Vowel V_ε)] [] :^ ShouldElide :^ NoInitialAspiration)
  , testDest "ὑπ’"
    (CoreWord HasInitialAspiration [Syllable [] (VS_Vowel V_υ)] [CR_π] :^ IsElided :^ NoInitialAspiration)
    (CoreWord HasInitialAspiration [Syllable [] (VS_Vowel V_υ), Syllable [CR_π] (VS_Vowel V_ο)] [] :^ ShouldElide :^ NoInitialAspiration)
  , testDest "ὑφ’"
    (CoreWord HasInitialAspiration [Syllable [] (VS_Vowel V_υ)] [CR_φ] :^ IsElided :^ HasInitialAspiration)
    (CoreWord HasInitialAspiration [Syllable [] (VS_Vowel V_υ), Syllable [CR_π] (VS_Vowel V_ο)] [] :^ ShouldElide :^ HasInitialAspiration)
  ]
  where
  testDest n vs ds = testRoundDest n Round.shouldElide vs ds

main :: IO ()
main =
  let
    modulesPath = "../modules"
    -- modulesPath = "../.."
  in defaultMain
  [ coreWordPhonemeRound
  , longestPrefixGroup
  , allSplitsListGroup
  , uniqueFormsGroup
  , shouldElideGroup
  , testGroupStages "morph stage" show Stage.morph id (pure <$> Serialize.readScript modulesPath)
  ]
