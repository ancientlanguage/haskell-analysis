module Grammar.Greek.Script.Around.VocalicSyllable where

import Control.Lens (over, _2)
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

vocalicSyllable
  :: a
  -> Around Void Void
    [Vowel :* Maybe SyllabicMark :* a]
    [VocalicSyllable :* a]
vocalicSyllable defaultA = makeIdAround to from
  where
  to = over (traverse . _2) snd . foldr toFold []

  toFold (v1, (Nothing, _)) ((VS_Vowel v2, m2@(Nothing, _)) : xs)
    | Just d <- tryDiphthong v1 v2
    = (VS_Diphthong d, m2) : xs
  toFold (v, m@(Just S_IotaSubscript, _)) xs
    | Just d <- tryImproperDiphthong v
    = (VS_ImproperDiphthong d, m) : xs
  toFold (v, m) xs = (VS_Vowel v, m) : xs

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

  from = foldr fromFold []

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
  fromFold (VS_Diphthong d, a) xs = consDiphthong (diphthongVowels (Nothing, defaultA) (Nothing, a) d) xs

  consDiphthong
    :: (Vowel :* Maybe SyllabicMark :* a, Vowel :* Maybe SyllabicMark :* a)
    -> [Vowel :* Maybe SyllabicMark :* a]
    -> [Vowel :* Maybe SyllabicMark :* a]

  -- Ἁλληλουϊά, εὐποιΐας
  consDiphthong ((v1, q1), (v2, q2)) ((v3, (Nothing, a3)) : xs)
    | isIotaUpsilon v3
    = (v1, q1) : (v2, q2) : (v3, (Just S_Diaeresis, a3)) : xs

  consDiphthong (x1, x2) xs = x1 : x2 : xs

  isIotaUpsilon :: Vowel -> Bool
  isIotaUpsilon V_ι = True
  isIotaUpsilon V_υ = True
  isIotaUpsilon _ = False

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
