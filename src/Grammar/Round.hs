module Grammar.Round where

import Data.Either.Validation
import Data.Void
import Grammar.CommonTypes
import Control.Lens (over, _2)

data Round e1 e2 a b = Round
  { roundTo :: a -> Validation [e1] b
  , roundFrom :: b -> Validation [e2] a
  }

makeIdRound :: (a -> b) -> (b -> a) -> Round Void Void a b
makeIdRound f g = Round (Success . f) (Success . g)

type ParseRound e = Round e Void
makeToValidationRound :: (a -> Validation e b) -> (b -> a) -> Round e Void a b
makeToValidationRound f g = Round (over _Failure pure . f) (Success . g)

joinValidation :: Validation [e1] (Validation [e2] a) -> Validation [e1 :+ e2] a
joinValidation (Failure es) = Failure (fmap Left es)
joinValidation (Success (Failure es)) = Failure (fmap Right es)
joinValidation (Success (Success x)) = Success x

joinRound
  :: Round e1 e2 a b
  -> Round e3 e4 b c
  -> Round (e1 :+ e3) (e4 :+ e2) a c
joinRound (Round a_b b_a) (Round b_c c_b) = Round a_c c_a
  where
  a_c = joinValidation . over _Success b_c . a_b
  c_a = joinValidation . over _Success b_a . c_b

joinValidation'
  :: Validation [q :* e1] (Validation [q :* e2] a)
  -> Validation [q :* (e1 :+ e2)] a
joinValidation' (Failure es) = Failure (over (traverse . _2) Left es)
joinValidation' (Success (Failure es)) = Failure (over (traverse . _2) Right es)
joinValidation' (Success (Success x)) = Success x

joinRound'
  :: Round (q :* e1) (q :* e2) a b
  -> Round (q :* e3) (q :* e4) b c
  -> Round (q :* (e1 :+ e3)) (q :* (e4 :+ e2)) a c
joinRound' (Round a_b b_a) (Round b_c c_b) = Round a_c c_a
  where
  a_c = joinValidation' . over _Success b_c . a_b
  c_a = joinValidation' . over _Success b_a . c_b

(<+>)
  :: Round (q :* e1) (q :* e2) a b
  -> Round (q :* e3) (q :* e4) b c
  -> Round (q :* (e1 :+ e3)) (q :* (e4 :+ e2)) a c
(<+>) = joinRound'
infixr 6 <+>

sumAssocLeft :: Round Void Void (a :+ (b :+ c)) ((a :+ b) :+ c)
sumAssocLeft = makeIdRound to from
  where
  to (Left x) = Left (Left x)
  to (Right (Left y)) = Left (Right y)
  to (Right (Right z)) = Right z

  from (Left (Left x)) = Left x
  from (Left (Right y)) = Right (Left y)
  from (Right z) = Right (Right z)

prodAssocLeft :: Round Void Void (a :* (b :* c)) ((a :* b) :* c)
prodAssocLeft = makeIdRound to from
  where
  to (x, (y, z)) = ((x, y), z)
  from ((x, y), z) = (x, (y, z))

distLeftSumOverProd :: Round Void Void ((a :+ b) :* c) ((a :* c) :+ (b :* c))
distLeftSumOverProd = makeIdRound to from
  where
  to (Left a, c) = Left (a, c)
  to (Right b, c) = Right (b, c)
  from (Left (a, c)) = (Left a, c)
  from (Right (b, c)) = (Right b, c)

distRightSumOverProd :: Round Void Void (a :* (b :+ c)) ((a :* b) :+ (a :* c))
distRightSumOverProd = makeIdRound to from
  where
  to (a, Left b) = Left (a, b)
  to (a, Right c) = Right (a, c)
  from (Left (a, b)) = (a, Left b)
  from (Right (a, c)) = (a, Right c)

groupSums :: Round Void Void [a :+ b] [[a] :+ [b]]
groupSums = makeIdRound to from
  where
  to = foldr go []
    where
    go (Left a) [] = [Left [a]]
    go (Right b) [] = [Right [b]]
    go (Left a) (Left as : xs) = Left (a : as) : xs
    go (Left a) xs@(Right _ : _) = Left [a] : xs
    go (Right b) xs@(Left _ : _) = Right [b] : xs
    go (Right b) (Right bs : xs) = Right (b : bs) : xs
  from = concatMap fromItem
  fromItem (Left as) = fmap Left as
  fromItem (Right bs) = fmap Right bs

ungroupSums :: Round Void Void [[a] :+ [b]] [a :+ b]
ungroupSums = Round (roundFrom groupSums) (roundTo groupSums)

groupRight :: Round Void Void [a :+ b] ([b] :* [a :* [b]])
groupRight = makeIdRound to from
  where
  to = foldr go ([], [])
  go (Left x) (ms, ys) = ([], (x, ms) : ys)
  go (Right m) (ms, ys) = (m : ms, ys)

  from (bs, xs) = fmap Right bs ++ concatMap (\(x, ms) -> Left x : fmap Right ms) xs

groupLeft :: Round Void Void [a :+ b] ([[a] :* b] :* [a])
groupLeft = makeIdRound to from
  where
  to = foldr go ([], [])
  go (Left a) ([], as) = ([], a : as)
  go (Left a) ((as', b) : xs, as) = ((a : as', b) : xs, as)
  go (Right b) (xs, as) = (([], b) : xs, as)

  from (xs, as) = concatMap (\(as', b) -> fmap Left as' ++ [Right b]) xs ++ fmap Left as

swapSum :: Round Void Void (a :+ b) (b :+ a)
swapSum = makeIdRound to from
  where
  to (Left a) = Right a
  to (Right b) = Left b
  from (Left b) = Right b
  from (Right a) = Left a
