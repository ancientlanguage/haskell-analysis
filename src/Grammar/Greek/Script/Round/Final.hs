module Grammar.Greek.Script.Round.Final where

import Control.Lens (over)
import Data.Either.Validation
import Grammar.Round
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidFinals
  = NonFinalSigmaAtEnd
  | FinalInMedialPosition Letter
  deriving (Show)

final :: RoundFwd [InvalidFinals] [(Letter :* Final) :* a] [Letter :* a]
final = makeRoundFwd to from
  where
  to xs = over _Success reverse $ case reverse xs of
    [] -> Success []
    ((q, a) : xs') -> pure (:) <*> pureFst a (checkFinal q) <*> ensureMedials xs'

  pureFst a mq = pure (\x -> (x, a)) <*> mq

  checkFinal (L_σ, NotFinal) = Failure [NonFinalSigmaAtEnd]
  checkFinal (l@L_σ, IsFinal) = Success l
  checkFinal (l, _) = Success l

  ensureMedials [] = Success []
  ensureMedials ((q, a) : xs) = pure (:) <*> pureFst a (check q) <*> ensureMedials xs
    where
    check (l, NotFinal) = Success l
    check (l, IsFinal) = Failure [FinalInMedialPosition l]

  from xs = reverse $ case reverse xs of
    [] -> []
    ((l, a) : xs') -> ((l, IsFinal), a) : fmap (\(l', a') -> ((l', NotFinal), a')) xs'
