module Grammar.Greek.Script.Around.Accent where

import Control.Lens (over, _1)
import Data.Either.Validation
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
accent = Around (over _Failure pure . to) (over _Failure pure . from)
  where
  to (ss, hp) = (\a -> (fmap fst ss, (a, hp))) <$> acc
    where
    acc = toPair (onlyAccentsWithReverseIndex $ getAccents ss, hp)

  standardTo a p = Success $ Just (a, (p, (NoForceAcute, NoExtraAccents)))
  extraTo a p = Success $ Just (a, (p, (NoForceAcute, SingleExtraAccent)))
  forceTo a p = Success $ Just (a, (p, (DoForceAcute, NoExtraAccents)))

  toPair ([], _) = Success Nothing
  toPair ([(0,A_Acute)], HasWordPunctuation) = standardTo AW_Acute Ultima
  toPair ([(0,A_Acute)], NoWordPunctuation) = forceTo AW_Acute Ultima
  toPair ([(0,A_Grave)], NoWordPunctuation) = standardTo AW_Acute Ultima
  toPair ([(0,A_Circumflex)], _) = standardTo AW_Circumflex Ultima
  toPair ([(1,A_Acute)], _) = standardTo AW_Acute Penult
  toPair ([(1,A_Circumflex)], _) = standardTo AW_Circumflex Penult
  toPair ([(1,A_Circumflex),(0,A_Acute)], NoWordPunctuation) = extraTo AW_Circumflex Penult
  toPair ([(2,A_Acute)], _) = standardTo AW_Acute Antepenult
  toPair ([(2,A_Acute),(0,A_Acute)], NoWordPunctuation) = extraTo AW_Acute Antepenult
  toPair x = Failure $ InvalidAccent x

  from = over (_Success . _1) reverse . fromTriple . over _1 (reverse . fmap (\x -> (x, Nothing)))

  fromTriple (xs, (Nothing, hp)) = Success (xs, hp)
  fromTriple ((s, _) : xs, (Just (AW_Acute, (Ultima, (NoForceAcute, NoExtraAccents))), hp@HasWordPunctuation)) = Success ((s, Just A_Acute) : xs, hp)
  fromTriple ((s, _) : xs, (Just (AW_Acute, (Ultima, (DoForceAcute, NoExtraAccents))), hp@NoWordPunctuation)) = Success ((s, Just A_Acute) : xs, hp)
  fromTriple ((s, _) : xs, (Just (AW_Acute, (Ultima, (NoForceAcute, NoExtraAccents))), hp@NoWordPunctuation)) = Success ((s, Just A_Grave) : xs, hp)
  fromTriple ((s, _) : xs, (Just (AW_Circumflex, (Ultima, (NoForceAcute, NoExtraAccents))), hp)) = Success ((s, Just A_Circumflex) : xs, hp)
  fromTriple (x1 : (s2, _) : xs, (Just (AW_Acute, (Penult, (NoForceAcute, NoExtraAccents))), hp)) = Success (x1 : (s2, Just A_Acute) : xs, hp)
  fromTriple (x1 : (s2, _) : xs, (Just (AW_Circumflex, (Penult, (NoForceAcute, NoExtraAccents))), hp)) = Success (x1 : (s2, Just A_Circumflex) : xs, hp)
  fromTriple ((s1, _) : (s2, _) : xs, (Just (AW_Circumflex, (Penult, (NoForceAcute, SingleExtraAccent))), hp@NoWordPunctuation)) = Success ((s1, Just A_Acute) : (s2, Just A_Circumflex) : xs, hp)
  fromTriple (x1 : x2 : (s3, _) : xs, (Just (AW_Acute, (Antepenult, (NoForceAcute, NoExtraAccents))), hp)) = Success (x1 : x2 : (s3, Just A_Acute) : xs, hp)
  fromTriple ((s1, _) : x2 : (s3, _) : xs, (Just (AW_Acute, (Antepenult, (NoForceAcute, SingleExtraAccent))), hp@NoWordPunctuation)) = Success ((s1, Just A_Acute) : x2 : (s3, Just A_Acute) : xs, hp)
  fromTriple (_, x) = Failure $ InvalidAccentProps x

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
