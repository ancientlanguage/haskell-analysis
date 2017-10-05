import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Grammar.Common.List
import Grammar.Common.Types

emptyUnitList :: [()]
emptyUnitList = []

main :: IO ()
main = defaultMain
  [ testGroup "contextualize"
    [ testCase "empty 0" $ assertEqual "emptyUnit" [] $ contextualize 0 emptyUnitList
    , testCase "empty 3" $ assertEqual "emptyUnit" [] $ contextualize 3 emptyUnitList
    , testCase "int 0" $ assertEqual "1,2,3" [(1, ([], [])), (2, ([], [])), (3, ([], []))] $ contextualize 0 [1,2,3]
    , testCase "int 1" $ assertEqual "1,2,3" [(1, ([], [2])), (2, ([1], [3])), (3, ([2], []))] $ contextualize 1 [1,2,3]
    , testCase "int 3" $ assertEqual "1,2,3" [(1, ([], [2,3])), (2, ([1], [3])), (3, ([2,1], []))] $ contextualize 3 [1,2,3]
    ]
  , testGroup "groupPairs"
    [ testCase "empty" $ assertEqual "empty" [] $ groupPairs ([] :: [((), ())])
    , testCase "singletons" $ assertEqual "singletons" [(1, ['a']), (2, ['b']), (3, ['c'])] $ groupPairs [(1,'a'),(2,'b'),(3,'c')]
    , testCase "consecutive groups" $ assertEqual "groups"
      [(1, ['a','b','c']), (2, ['d','e']), (3, ['f'])] $
      groupPairs [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(3,'f')]
    , testCase "non-consecutive groups" $ assertEqual "groups"
      [(1, ['a','b','c','g','h']), (2, ['d','e']), (3, ['f'])] $
      groupPairs [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(3,'f'),(1,'g'),(1,'h')]
    ]
  , testGroup "groupConsecutivePairs"
    [ testCase "empty" $ assertEqual "empty" [] $ groupConsecutivePairs ([] :: [((), ())])
    , testCase "singletons" $ assertEqual "singletons" [(1, ['a']), (2, ['b']), (3, ['c'])] $ groupConsecutivePairs [(1,'a'),(2,'b'),(3,'c')]
    , testCase "consecutive groups" $ assertEqual "groups"
      [(1, ['a','b','c']), (2, ['d','e']), (3, ['f'])] $
      groupConsecutivePairs [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(3,'f')]
    , testCase "non-consecutive separate groups" $ assertEqual "groups"
      [(1, ['a','b','c']), (2, ['d','e']), (3, ['f']),(1,['g','h'])] $
      groupConsecutivePairs [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(3,'f'),(1,'g'),(1,'h')]
    ]
  ]
