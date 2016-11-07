import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Grammar.CommonTypes
import Grammar.Contextualize

emptyUnitList :: [()]
emptyUnitList = []

main :: IO ()
main = defaultMain
  [ testGroup "contextualize"
    [ testCase "empty 0" (assertEqual "emptyUnit" [] (contextualize 0 emptyUnitList))
    , testCase "empty 3" (assertEqual "emptyUnit" [] (contextualize 3 emptyUnitList))
    , testCase "int 0" (assertEqual "1,2,3" [(1, ([], [])), (2, ([], [])), (3, ([], []))] (contextualize 0 [1,2,3]))
    , testCase "int 1" (assertEqual "1,2,3" [(1, ([], [2])), (2, ([1], [3])), (3, ([2], []))] (contextualize 1 [1,2,3]))
    , testCase "int 3" (assertEqual "1,2,3" [(1, ([], [2,3])), (2, ([1], [3])), (3, ([2,1], []))] (contextualize 3 [1,2,3]))
    ]
  ]
