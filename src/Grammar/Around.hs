module Grammar.Around where

import Data.Void
import Grammar.CommonTypes

data Around e1 e2 a b = Around
  { aroundTo :: a -> e1 :+ b
  , aroundFrom :: b -> e2 :+ a
  }

type IdAround = Around Void Void
makeIdAround :: (a -> b) -> (b -> a) -> IdAround a b
makeIdAround f g = Around (Right . f) (Right . g)

type ParseAround e = Around e Void
makeParseAround :: (a -> e :+ b) -> (b -> a) -> ParseAround e a b
makeParseAround f g = Around f (Right . g)
