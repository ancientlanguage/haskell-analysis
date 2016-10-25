module Grammar.Greek.Script.Around.Final where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidFinals = InvalidFinals [Letter :* Case :* Final]
  deriving (Show)

final :: Around InvalidFinals Void [(Letter :* Case :* Final) :* a] [(Letter :* Case) :* a]
final = makeToValidationAround (fixTo . to) from
  where
  fixTo
    = over _Success reverse
    . over _Failure InvalidFinals
  to xs = case reverse xs of
    [] -> Success []
    (x : xs') -> pure (:) <*> check x <*> ensureMedials xs'
    where
    check ((l@L_σ, (c@Lowercase, IsFinal)), a) = Success ((l, c), a)
    check ((l@L_σ, (c@Uppercase, FinalNotSupported)), a) = Success ((l, c), a)
    check ((l, (c, FinalNotSupported)), a) = Success ((l, c), a)
    check (x', _) = Failure [x']
  ensureMedials [] = Success []
  ensureMedials (x : xs) = pure (:) <*> check x <*> ensureMedials xs
    where
    check ((l@L_σ, (c@Lowercase, NotFinal)), a) = Success ((l, c), a)
    check ((l@L_σ, (c@Uppercase, FinalNotSupported)), a) = Success ((l, c), a)
    check ((l, (c, FinalNotSupported)), a) = Success ((l, c), a)
    check (x', _) = Failure [x']

  from xs = reverse $ case reverse xs of
    [] -> []
    (x : xs') -> makeFinal x : fmap makeMedial xs'  
    where
    makeFinal ((l@L_σ, c@Lowercase), a) = ((l, (c, IsFinal)), a)
    makeFinal ((l@L_σ, c@Uppercase), a) = ((l, (c, FinalNotSupported)), a)
    makeFinal ((l, c), a) = ((l, (c, FinalNotSupported)), a)
    makeMedial ((l@L_σ, c@Lowercase), a) = ((l, (c, NotFinal)), a)
    makeMedial ((l@L_σ, c@Uppercase), a) = ((l, (c, FinalNotSupported)), a)
    makeMedial ((l, c), a) = ((l, (c, FinalNotSupported)), a)
