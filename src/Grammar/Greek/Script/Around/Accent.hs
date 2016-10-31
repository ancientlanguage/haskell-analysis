module Grammar.Greek.Script.Around.Accent where

import Control.Lens (toListOf, _2, _Just)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidAccent = InvalidAccent ([Int :* Accent] :* HasWordPunctuation)
  deriving (Show)

accent :: Around InvalidAccent Void
  ([c :* v :* Maybe Accent] :* HasWordPunctuation)
  ([c :* v] :* Maybe (WordAccent :* AccentPosition :* ForceAcute :* ExtraAccents) :* HasWordPunctuation)
accent = makeToValidationAround to from
  where
  to = undefined
  from = undefined

  handlePair ([], _) = Success Nothing
  handlePair ([(0,A_Acute)], HasWordPunctuation) = Success $ (AW_Acute, (Ultima, ()))
  handlePair ([(0,A_Acute)], NoWordPunctuation) = Success $ (AW_Acute, (Ultima, ()))
  handlePair ([(0,A_Grave)], NoWordPunctuation) = Success
  handlePair ([(0,A_Circumflex)], _) = Success
  handlePair ([(1,A_Acute)], _) = Success
  handlePair ([(1,A_Circumflex)], _) = Success
  handlePair ([(1,A_Circumflex),(0,A_Acute)], _) = Success
  handlePair ([(2,A_Acute)], _) = Success
  handlePair ([(2,A_Acute),(0,A_Acute)], _) = Success
  handlePair x = Failure $ InvalidAccent x

getAccents :: [ c :* v :* Maybe Accent ] -> [Maybe Accent]
getAccents = over traverse (view (_2 . _2))

onlyAccentsWithReverseIndex :: [Maybe Accent] -> [(Int, Accent)]
onlyAccentsWithReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe Accent)] -> [(Int, Accent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []
