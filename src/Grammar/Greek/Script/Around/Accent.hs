module Grammar.Greek.Script.Around.Accent where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidAccent = InvalidAccent ([Int :* Accent] :* HasWordPunctuation)
  deriving (Show)

data InvalidAccentProps = InvalidAccentProps (Maybe (WordAccent :* AccentPosition :* ForceAcute :* ExtraAccents) :* HasWordPunctuation)
  deriving (Show)

accent :: Around InvalidAccent InvalidAccentProps
  ([s :* Maybe Accent] :* HasWordPunctuation)
  ([s] :* Maybe (WordAccent :* AccentPosition :* ForceAcute :* ExtraAccents) :* HasWordPunctuation)
accent = makeToValidationAround to from
  where
  to (ss, hp) = (\a -> (fmap fst ss, (a, hp))) <$> acc
    where
    acc = toPair (onlyAccentsWithReverseIndex $ getAccents ss, hp)

  standardTo a p = Success $ Just (a, (p, (NoForceAcute, NoExtraAccents)))
  specialTo a p f e = Success $ Just (a, (p, (f, e)))

  toPair ([], _) = Success Nothing
  toPair ([(0,A_Acute)], HasWordPunctuation) = standardTo AW_Acute Ultima
  toPair ([(0,A_Acute)], NoWordPunctuation) = specialTo AW_Acute Ultima DoForceAcute NoExtraAccents
  toPair ([(0,A_Grave)], NoWordPunctuation) = standardTo AW_Acute Ultima
  toPair ([(0,A_Circumflex)], _) = standardTo AW_Circumflex Ultima
  toPair ([(1,A_Acute)], _) = standardTo AW_Acute Penult
  toPair ([(1,A_Circumflex)], _) = standardTo AW_Circumflex Penult
  toPair ([(1,A_Circumflex),(0,A_Acute)], NoWordPunctuation) = specialTo AW_Circumflex Penult NoForceAcute SingleExtraAccent
  toPair ([(2,A_Acute)], _) = standardTo AW_Acute Antepenult
  toPair ([(2,A_Acute),(0,A_Acute)], NoWordPunctuation) = specialTo AW_Acute Antepenult NoForceAcute SingleExtraAccent
  toPair x = Failure $ InvalidAccent x

  from = undefined

getAccents :: [ s :* Maybe Accent ] -> [Maybe Accent]
getAccents = over traverse snd

onlyAccentsWithReverseIndex :: [Maybe Accent] -> [(Int, Accent)]
onlyAccentsWithReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe Accent)] -> [(Int, Accent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []
