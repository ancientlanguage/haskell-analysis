module Grammar.Greek.Script.Rounds.VocalicSyllable where

import Control.Lens (over, _1, _2, toListOf)
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data BreakDiphthong = DoBreakDiphthong | NoBreakDiphthong

vocalicSyllable
  :: RoundId
    [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]
    ([VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :* DiaeresisConvention)
vocalicSyllable = RoundId to from
  where
  to xs = sylls :^ conv
    where
    combined = foldr toFold [] xs
    sylls = over (traverse . _2) (\(_, (x2, (x3, _))) -> (x2, x3)) combined
    conv = mergeDiaeresisConventions $ toListOf (traverse . _2 . _2 . _2 . _2) combined

  mergeAccentBreaksDiphthong :: AccentBreaksDiphthong -> AccentBreaksDiphthong -> AccentBreaksDiphthong
  mergeAccentBreaksDiphthong AccentBreaksDiphthong _ = AccentBreaksDiphthong
  mergeAccentBreaksDiphthong _ x = x

  mergeUselessDiaeresis :: UselessDiaeresis -> UselessDiaeresis -> UselessDiaeresis
  mergeUselessDiaeresis UselessDiaeresis _ = UselessDiaeresis
  mergeUselessDiaeresis _ x = x

  mergeDiaeresisConventions :: [DiaeresisConvention] -> DiaeresisConvention
  mergeDiaeresisConventions = foldr go basicDiaeresisConvention
    where
    go (DiaeresisConvention x1 y1) (DiaeresisConvention x2 y2) = DiaeresisConvention
      (mergeAccentBreaksDiphthong x1 x2)
      (mergeUselessDiaeresis y1 y2)

  toFold
    (v1 :^ Nothing :^ Nothing :^ Nothing)
    ((VS_Vowel v2 :^ s2@Nothing :^ a2 :^ b2 :^ c) : xs)
    | Just d <- tryDiphthong v1 v2
    = (VS_Diphthong d :^ s2 :^ a2 :^ b2 :^ basicDiaeresisConvention) : xs
  toFold (v :^ s@(Just S_IotaSubscript) :^ a :^ b) xs
    | Just d <- tryImproperDiphthong v
    = (VS_ImproperDiphthong d :^ s :^ a :^ b :^ basicDiaeresisConvention) : xs
  toFold (v, (s, (a, b))) xs = (VS_Vowel v :^ s :^ a :^ b :^ basicDiaeresisConvention) : xs

  isIotaUpsilon :: Vowel -> Bool
  isIotaUpsilon V_ι = True
  isIotaUpsilon V_υ = True
  isIotaUpsilon _ = False

  isNotIotaSubscript :: Maybe SyllabicMark -> Bool
  isNotIotaSubscript (Just S_IotaSubscript) = False
  isNotIotaSubscript (Just S_Diaeresis) = True
  isNotIotaSubscript Nothing = True

  tryDiphthong :: Vowel -> Vowel -> Maybe Diphthong
  tryDiphthong V_α V_ι = Just D_αι
  tryDiphthong V_α V_υ = Just D_αυ
  tryDiphthong V_ε V_ι = Just D_ει
  tryDiphthong V_ε V_υ = Just D_ευ
  tryDiphthong V_η V_υ = Just D_ηυ
  tryDiphthong V_ο V_ι = Just D_οι
  tryDiphthong V_ο V_υ = Just D_ου
  tryDiphthong V_υ V_ι = Just D_υι
  tryDiphthong V_ω V_ι = Just D_ωι
  tryDiphthong _ _ = Nothing

  tryImproperDiphthong :: Vowel -> Maybe ImproperDiphthong
  tryImproperDiphthong V_α = Just I_α
  tryImproperDiphthong V_η = Just I_η
  tryImproperDiphthong V_ω = Just I_ω
  tryImproperDiphthong _ = Nothing

  from (ss, _) = from' ss
  from' = foldr fromFold []

  -- Δαυίδ, Νινευῖται
  fromFold (VS_Vowel v1, a1) ((v2, (Nothing, a2)) : (v3, (Nothing, a3)) : xs)
    | isIotaUpsilon v2
    , isIotaUpsilon v3
    = (v1, (Nothing, a1)) : (v2, (Nothing, a2)) : (v3, (Nothing, a3)) : xs

  -- Μωϋσῆς, διϋλίζοντες, πρωῒ, διϊσχυρίζετο, Ἠσαΐου
  fromFold (VS_Vowel v1, a1) ((v2, (Nothing, a2)) : xs)
    | isIotaUpsilon v2
    = (v1, (Nothing, a1)) : (v2, (Just S_Diaeresis, a2)) : xs

  fromFold (VS_Vowel v, a) xs = (v, (Nothing, a)) : xs
  fromFold (VS_ImproperDiphthong v, a) xs = (improperDiphthongVowel v, (Just S_IotaSubscript, a)) : xs
  fromFold (VS_Diphthong d, a) xs = consDiphthong (diphthongVowels (Nothing, (Nothing, Nothing)) (Nothing, a) d) xs

  consDiphthong
    :: (Vowel :* Maybe SyllabicMark :* a, Vowel :* Maybe SyllabicMark :* a)
    -> [Vowel :* Maybe SyllabicMark :* a]
    -> [Vowel :* Maybe SyllabicMark :* a]

  -- Ἁλληλουϊά, εὐποιΐας
  consDiphthong ((v1, q1), (v2, q2)) ((v3, (Nothing, a3)) : xs)
    | isIotaUpsilon v3
    = (v1, q1) : (v2, q2) : (v3, (Just S_Diaeresis, a3)) : xs

  consDiphthong (x1, x2) xs = x1 : x2 : xs

  diphthongVowels :: q -> q -> Diphthong -> (Vowel :* q, Vowel :* q)
  diphthongVowels a1 a2 D_αι = ((V_α, a1), (V_ι, a2))
  diphthongVowels a1 a2 D_αυ = ((V_α, a1), (V_υ, a2))
  diphthongVowels a1 a2 D_ει = ((V_ε, a1), (V_ι, a2))
  diphthongVowels a1 a2 D_ευ = ((V_ε, a1), (V_υ, a2))
  diphthongVowels a1 a2 D_ηυ = ((V_η, a1), (V_υ, a2))
  diphthongVowels a1 a2 D_οι = ((V_ο, a1), (V_ι, a2))
  diphthongVowels a1 a2 D_ου = ((V_ο, a1), (V_υ, a2))
  diphthongVowels a1 a2 D_υι = ((V_υ, a1), (V_ι, a2))
  diphthongVowels a1 a2 D_ωι = ((V_ω, a1), (V_ι, a2))

  improperDiphthongVowel :: ImproperDiphthong -> Vowel
  improperDiphthongVowel I_α = V_α
  improperDiphthongVowel I_η = V_η
  improperDiphthongVowel I_ω = V_ω
