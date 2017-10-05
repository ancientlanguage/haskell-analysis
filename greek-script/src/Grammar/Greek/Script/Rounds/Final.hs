module Grammar.Greek.Script.Rounds.Final where

import Control.Lens (over, _1)
import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidFinals
  = NonFinalSigmaAtEnd
  | FinalInMedialPosition Letter
  deriving (Show)

final :: RoundFwd [InvalidFinals] ([(Letter :* Final) :* a] :* Elision) ([Letter :* a] :* Elision)
final = makeRoundFwd to from
  where
  to (xs, el@NotElided) = toHandleFinal xs el
  to (xs, el@Aphaeresis) = toHandleFinal xs el
  to (xs, el@IsElided) = pure (:^ el) <*> (ensureMedials xs)

  toHandleFinal xs el = over (_Success . _1) reverse $ case reverse xs of
    [] -> Success $ [] :^ el
    ((q, a) : xs') -> pure (:^ el) <*> (pure (:) <*> pureFst a (checkFinal q) <*> ensureMedials xs')

  pureFst a mq = pure (\x -> (x, a)) <*> mq

  checkFinal (L_σ, NotFinal) = Failure [NonFinalSigmaAtEnd]
  checkFinal (l@L_σ, IsFinal) = Success l
  checkFinal (l, _) = Success l

  ensureMedials [] = Success []
  ensureMedials ((q, a) : xs) = pure (:) <*> pureFst a (check q) <*> ensureMedials xs
    where
    check (l, NotFinal) = Success l
    check (l, IsFinal) = Failure [FinalInMedialPosition l]

  from (xs, el@NotElided) = setWordFinal xs :^ el
  from (xs, el@Aphaeresis) = setWordFinal xs :^ el
  from (xs, el@IsElided) = fmap (\(l, a) -> ((l :^ NotFinal), a)) xs :^ el

  setWordFinal xs = reverse $ case reverse xs of
    [] -> []
    ((l, a) : xs') -> ((l, IsFinal), a) : fmap (\(l', a') -> ((l', NotFinal), a')) xs'
