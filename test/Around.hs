module Around where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Either.Validation
import Grammar.Around

testAround
  :: (Show a, Show e1, Show e2)
  => String
  -> Around e1 e2 a b
  -> [a]
  -> [Test]
testAround label (Around f g) = fmap testToFrom
  where
  testToFrom x = testCase (label ++ " -- to/from -- " ++ show x) $ do
    case f x of
      Failure e1 -> assertFailure $ "to failure: " ++ show e1
      Success y ->
        case g y of
          Failure e2 -> assertFailure $ "from failure: " ++ show e2
          Success _ -> return ()
