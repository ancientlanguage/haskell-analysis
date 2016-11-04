module Grammar.Greek.Script.Rounds.MarkGroups where

import Data.Either.Validation
import Grammar.Round
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InitialMarks = InitialMarks [Mark]
  deriving (Show)

markGroups :: RoundFwd InitialMarks [a :+ Mark] [a :* [Mark]]
markGroups = makeRoundFwd to from
  where
  to xs = case foldr go ([], []) xs of
    ([], ys) -> Success ys
    (ms@(_ : _), _) -> Failure $ InitialMarks ms
  go (Left x) (ms, ys) = ([], (x, ms) : ys)
  go (Right m) (ms, ys) = (m : ms, ys)

  from = concatMap (\(x, ms) -> Left x : fmap Right ms)
