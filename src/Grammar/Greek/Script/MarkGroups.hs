module Grammar.Greek.Script.MarkGroups where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InitialMarks = InitialMarks [Mark] 
  deriving (Show)

markGroups :: Around InitialMarks Void [a :+ Mark] [a :* [Mark]]
markGroups = Around (over _Failure pure . to) (Success . from)
  where
  to xs = case foldr go ([], []) xs of
    ([], ys) -> Success ys
    (ms@(_ : _), _) -> Failure $ InitialMarks ms
  go (Left x) (ms, ys) = ([], (x, ms) : ys)
  go (Right m) (ms, ys) = (m : ms, ys)

  from = concatMap (\(x, ms) -> Left x : fmap Right ms)
