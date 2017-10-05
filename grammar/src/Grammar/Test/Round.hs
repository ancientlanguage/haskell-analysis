module Grammar.Test.Round where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Grammar.Common.Round

testList
  :: (Show a)
  => String
  -> (String -> a -> Test)
  -> [a]
  -> [Test]
testList mainLabel f = fmap (\x -> f (mainLabel ++ " -- " ++ show x) x)

testRound
  :: (Eq a, Show a, Show e1, Show e2)
  => Round e1 e2 a b
  -> String
  -> a
  -> Test
testRound (Round f g) label x = testCase label $ do
  case f x of
    Failure e1 -> assertFailure $ "to failure: " ++ show e1
    Success y ->
      case g y of
        Failure e2 -> assertFailure $ "from failure: " ++ show e2
        Success z -> assertEqual "data loss" x z

testRoundFwd
  :: (Eq a, Show a, Show e)
  => RoundFwd e a b
  -> String
  -> a
  -> Test
testRoundFwd (RoundFwd f g) label x = testCase label $ do
  case f x of
    Failure e1 -> assertFailure $ "to failure: " ++ show e1
    Success y -> assertEqual "data loss" x (g y)

testRoundId
  :: (Eq a, Show a)
  => RoundId a b
  -> String
  -> a
  -> Test
testRoundId (RoundId f g) label x = testCase label $ assertEqual "data loss" x (g . f $ x)

testRoundIdDest
  :: (Eq a, Eq b, Show a, Show b)
  => RoundId a b
  -> String
  -> a
  -> b
  -> Test
testRoundIdDest (RoundId f g) label x y = testCase label $ do
  _ <- assertEqual "destination" (f x) y
  assertEqual "data loss" x (g . f $ x)

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
