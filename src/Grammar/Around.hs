module Grammar.Around where

import Data.Either.Validation
import Data.Void
import Grammar.CommonTypes
import Control.Lens (over, _1, _2)

data Around e1 e2 a b = Around
  { aroundTo :: a -> Validation [e1] b
  , aroundFrom :: b -> Validation [e2] a
  }

type IdAround = Around Void Void
makeIdAround :: (a -> b) -> (b -> a) -> IdAround a b
makeIdAround f g = Around (Success . f) (Success . g)

type ParseAround e = Around e Void
makeParseAround :: (a -> Validation [e] b) -> (b -> a) -> ParseAround e a b
makeParseAround f g = Around f (Success . g)

data Stage e1 e2 a b a' = Stage
  { stageAround :: Around e1 e2 a b
  , stageForget :: a -> a'
  }

joinValidation :: Validation [e1] (Validation [e2] a) -> Validation [e1 :+ e2] a
joinValidation (Failure es) = Failure (fmap Left es)
joinValidation (Success (Failure es)) = Failure (fmap Right es)
joinValidation (Success (Success x)) = Success x

joinAround
  :: Around e1 e2 a b
  -> Around e3 e4 b c
  -> Around (e1 :+ e3) (e4 :+ e2) a c
joinAround (Around a_b b_a) (Around b_c c_b) = Around a_c c_a
  where
  a_c = joinValidation . over _Success b_c . a_b
  c_a = joinValidation . over _Success b_a . c_b

joinValidation'
  :: Validation [q :* e1] (Validation [q :* e2] a)
  -> Validation [q :* (e1 :+ e2)] a
joinValidation' (Failure es) = Failure (over (traverse . _2) Left es)
joinValidation' (Success (Failure es)) = Failure (over (traverse . _2) Right es)
joinValidation' (Success (Success x)) = Success x

joinAround'
  :: Around (q :* e1) (q :* e2) a b
  -> Around (q :* e3) (q :* e4) b c
  -> Around (q :* (e1 :+ e3)) (q :* (e4 :+ e2)) a c
joinAround' (Around a_b b_a) (Around b_c c_b) = Around a_c c_a
  where
  a_c = joinValidation' . over _Success b_c . a_b
  c_a = joinValidation' . over _Success b_a . c_b

aroundSumAssoc12_3 :: Around Void Void (a :+ (b :+ c)) ((a :+ b) :+ c)
aroundSumAssoc12_3 = Around (Success . to) (Success . from)
  where
  to (Left x) = Left (Left x)
  to (Right (Left y)) = Left (Right y)
  to (Right (Right z)) = Right z

  from (Left (Left x)) = Left x
  from (Left (Right y)) = Right (Left y)
  from (Right z) = Right (Right z)
