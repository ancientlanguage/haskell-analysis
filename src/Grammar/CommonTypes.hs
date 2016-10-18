module Grammar.CommonTypes
  ( (:*)
  , (:+)
  )
  where

type a :* b = (a, b)
type a :+ b = Either a b

infixr 6 :*
infixr 5 :+
