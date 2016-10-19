module Grammar.Around where

import Data.Either.Validation
import Data.Void

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

{-
-- (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
liftAround
  :: (a -> Validation [e1] a')
  -> (b -> Validation [e2] b')
  -> Around e1 e2 a b
  -> Around e1 e2 a' b'
liftAround f g (Around to from) = Around (\x -> f <$> to x) (\x -> g <$> from x) 
-}

data Stage e1 e2 a b c = Stage
  { stageAround :: Around e1 e2 a b
  , stageForget :: a -> c
  }
