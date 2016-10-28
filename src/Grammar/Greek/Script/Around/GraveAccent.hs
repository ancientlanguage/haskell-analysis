module Grammar.Greek.Script.Around.Breathing where

import Control.Lens (toListOf, _2, _Just)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidGraveAccent = InvalidGraveAccent
  deriving (Show)

graveAccent :: Around [InvalidGraveAccent] Void
  ([c :* v :* Maybe Accent] :* SentenceBoundary)
  ([c :* v :* Maybe BasicAccent] :* SentenceBoundary)
graveAccent = makeToValidationAround to from
  where
  to = undefined
  from = undefined

  accents = toListOf (traverse . _2 . _2 . _Just)
  isGrave A_Grave = True
  isGrave _ = False

  removeGrave A_Acute = AB_Acute
  removeGrave A_Grave = AB_Acute
  removeGrave A_Circumflex = AB_Circumflex
