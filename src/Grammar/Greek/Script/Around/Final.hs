module Grammar.Greek.Script.Around.Final where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidFinals = InvalidFinals [Letter :* Final] 
  deriving (Show)

final :: Around InvalidFinals Void [a :* Letter :* Final] [a :* Letter]
final = Around (over _Failure pure . to) (Success . from)
  where
    to = undefined
    from = undefined
