module Grammar.Greek.Script.Around.Capitalization where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidUppercase = InvalidUppercase [Letter]
  deriving (Show)

capitalization :: Around InvalidUppercase Void [(Letter :* Case) :* a] ([Letter :* a] :* Capitalization)
capitalization = makeToValidationAround (over _Failure InvalidUppercase . to) from
  where
  to [] = Success ([], NotCapitalized)
  to (((l, c), a) : ls) = pure prefix <*> ensureLowercase ls
    where
    prefix xs = ((l, a) : xs, cap c)
    cap Lowercase = NotCapitalized
    cap Uppercase = IsCapitalized

  ensureLowercase [] = Success []
  ensureLowercase (((l, Lowercase), a) : xs) = pure (:) <*> pure (l, a) <*> ensureLowercase xs
  ensureLowercase (((l, Uppercase), _) : xs) = pure (:) <*> Failure [l] <*> ensureLowercase xs

  from ([], IsCapitalized) = []
  from ((l, a) : ls, IsCapitalized) = ((l, Uppercase), a) : from (ls, NotCapitalized)
  from (ls, NotCapitalized) = fmap (\(l, a) -> ((l, Lowercase), a)) ls
