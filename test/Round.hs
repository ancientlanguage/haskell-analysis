module Round where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Grammar.Round

testRoundList
  :: (Eq a, Show a, Show e1, Show e2)
  => String
  -> Round e1 e2 a b
  -> [a]
  -> [Test]
testRoundList label a = fmap (\x -> testRound (label ++ " -- to/from -- " ++ show x) a x)

testRound
  :: (Eq a, Show a, Show e1, Show e2)
  => String
  -> Round e1 e2 a b
  -> a
  -> Test
testRound label (Round f g) x = testCase label $ do
  case f x of
    Failure e1 -> assertFailure $ "to failure: " ++ show e1
    Success y ->
      case g y of
        Failure e2 -> assertFailure $ "from failure: " ++ show e2
        Success z -> assertEqual "data loss" x z

testRoundDest
  :: (Eq a, Show a, Eq b, Show b, Show e1, Show e2)
  => String
  -> Round e1 e2 a b
  -> a
  -> b
  -> Test
testRoundDest label (Round f g) x yInput = testCase label $ do
  case f x of
    Failure e1 -> assertFailure $ "to failure: " ++ show e1
    Success y -> do
      _ <- assertEqual "destination" yInput y
      case g y of
        Failure e2 -> assertFailure $ "from failure: " ++ show e2
        Success z -> assertEqual "data loss" x z
