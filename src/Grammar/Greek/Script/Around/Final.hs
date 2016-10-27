module Grammar.Greek.Script.Around.Final where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidFinals = InvalidFinals [Letter :* Final]
  deriving (Show)

final :: Around InvalidFinals Void [(Letter :* Final) :* a] [Letter :* a]
final = makeToValidationAround (fixTo . to) from
  where
  fixTo
    = over _Success reverse
    . over _Failure InvalidFinals
  to xs = case reverse xs of
    [] -> Success []
    (((l, f), a) : xs') -> pure (:) <*> pure (l, a) <*> ensureMedials xs'
  ensureMedials [] = Success []
  ensureMedials (x : xs) = pure (:) <*> check x <*> ensureMedials xs
    where
    check ((l, NotFinal), a) = Success (l, a)
    check (x', _) = Failure [x']

  from xs = reverse $ case reverse xs of
    [] -> []
    ((l, a) : xs') -> ((l, IsFinal), a) : fmap (\(l', a') -> ((l', NotFinal), a')) xs'
