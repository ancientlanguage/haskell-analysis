module Grammar.Greek.Script.Rounds.VocalicSyllable where

import Control.Lens (over, _1, _2, toListOf)
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data BreakDiphthong = DoBreakDiphthong | NoBreakDiphthong

vocalicSyllable
  :: Capitalization
  -> RoundId
    ([Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing])
    ([VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :* DiaeresisConvention)
vocalicSyllable capTop = RoundId to from
  where
  to xs = sylls :^ conv
    where
    combined = foldr (toFold capTop) [] xs
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
    _
    (v1 :^ Nothing :^ Nothing :^ Nothing)
    ((VS_Vowel v2 :^ Nothing :^ a2 :^ b2 :^ c) : xs)
    | Just d <- tryDiphthong v1 v2
    = (VS_Diphthong d :^ Nothing :^ a2 :^ b2 :^ basicDiaeresisConvention) : xs

  -- ἰσχύι, πρώιμος, οἰσύινα
  toFold
    _
    (v1 :^ Nothing :^ a1@(Just _) :^ Nothing)
    ((VS_Vowel v2 :^ Nothing :^ Nothing :^ Nothing :^ c) : xs)
    | Just d <- tryDiphthong v1 v2
    = (VS_Vowel v1 :^ Nothing :^ a1 :^ Nothing :^ basicDiaeresisConvention)
    : (VS_Vowel v2 :^ Nothing :^ Nothing :^ Nothing :^ DiaeresisConvention AccentBreaksDiphthong EssentialDiaeresis)
    : xs

  -- ἀίδιον, ἐυξέστου, ὑικὸν ?
  -- εὐιπποτάτην, εὐιατότερα
  toFold
    _
    (v1 :^ Nothing :^ Nothing :^ b1@(Just _))
    ((VS_Vowel v2 :^ Nothing :^ a2 :^ Nothing :^ c) : xs)
    | Just d <- tryDiphthong v1 v2
    = (VS_Vowel v1 :^ Nothing :^ Nothing :^ b1 :^ basicDiaeresisConvention)
    : (VS_Vowel v2 :^ Nothing :^ a2 :^ Nothing :^ DiaeresisConvention AccentBreaksDiphthong EssentialDiaeresis)
    : xs

  -- Ἁλληλουϊά
  toFold
    _
    (v1 :^ Nothing :^ Nothing :^ Nothing)
    ( (VS_Vowel v2 :^ Nothing :^ a2 :^ b2 :^ c2)
      : (VS_Vowel v3 :^ s3@(Just S_Diaeresis) :^ a3 :^ b3 :^ DiaeresisConvention dc3 UselessDiaeresis)
      : xs
    )
    | Just d <- tryDiphthong v1 v2
    , Just _ <- tryDiphthong v2 v3
    = (VS_Diphthong d :^ Nothing :^ a2 :^ b2 :^ basicDiaeresisConvention)
    : (VS_Vowel v3 :^ s3 :^ a3 :^ b3 :^ DiaeresisConvention dc3 EssentialDiaeresis)
    : xs

  -- useless: Μωϋσῆς, διϋλίζοντες, διϊσχυρίζετο, διϊκνούμενος, εὐποιΐας
  -- essential: νηὶ, Διὶ
  toFold
    _
    (v1 :^ Nothing :^ Nothing :^ Nothing)
    ((VS_Vowel v2 :^ s2@(Just S_Diaeresis) :^ a2 :^ b2 :^ c) : xs)
    | Nothing <- tryDiphthong v1 v2
    = (VS_Vowel v1 :^ Nothing :^ Nothing :^ Nothing :^ basicDiaeresisConvention)
    : (VS_Vowel v2 :^ s2 :^ a2 :^ b2 :^ DiaeresisConvention AccentNotBreaksDiphthong UselessDiaeresis)
    : xs

  toFold
    _
    (v :^ s@(Just S_IotaSubscript) :^ a :^ b)
    xs
    | Just d <- tryImproperDiphthong v
    = (VS_ImproperDiphthong d :^ s :^ a :^ b :^ basicDiaeresisConvention) : xs

  toFold
    _
    (v, (s, (a, b)))
    xs
    = (VS_Vowel v :^ s :^ a :^ b :^ basicDiaeresisConvention) : xs

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

  from (ss, conv) = from' capTop conv ss
  from' cap conv = foldr (fromFold cap conv) []

  -- Δαυίδ, Νινευῖται
  fromFold
    _
    _
    (VS_Vowel v1, a1) ((v2, (Nothing, a2)) : (v3, (Nothing, a3)) : xs)
    | isIotaUpsilon v2
    , isIotaUpsilon v3
    = (v1, (Nothing, a1)) : (v2, (Nothing, a2)) : (v3, (Nothing, a3)) : xs

  -- ἰσχύι, πρώιμος, οἰσύινα
  fromFold
    _
    (DiaeresisConvention AccentBreaksDiphthong _)
    (VS_Vowel v1, (a1@(Just _), Nothing))
    ((v2, (Nothing, (Nothing, Nothing))) : xs)
    | Just _ <- tryDiphthong v1 v2
    = (v1 :^ Nothing :^ a1 :^ Nothing) : (v2 :^ Nothing :^ Nothing :^ Nothing) : xs

  -- ἀίδιον, ἐυξέστου
  fromFold
    _
    (DiaeresisConvention AccentBreaksDiphthong _)
    (VS_Vowel v1, (a1, b1@(Just _)))
    ((v2, (Nothing, q2)) : xs)
    | Just _ <- tryDiphthong v1 v2
    = (v1 :^ Nothing :^ a1 :^ b1) : (v2 :^ Nothing :^ q2) : xs

  fromFold
    _
    _
    (VS_Vowel v1, a1) ((v2, (Nothing, a2)) : xs)
    | Just _ <- tryDiphthong v1 v2
    = (v1, (Nothing, a1)) : (v2, (Just S_Diaeresis, a2)) : xs

  -- Μωϋσῆς, διϋλίζοντες, διϊσχυρίζετο, διϊκνούμενος, εὐποιΐας
  fromFold
    _
    (DiaeresisConvention _ UselessDiaeresis)
    (VS_Vowel v1, a1) ((v2, (Nothing, a2)) : xs)
    | isIotaUpsilon v2
    = (v1, (Nothing, a1)) : (v2, (Just S_Diaeresis, a2)) : xs

  fromFold
    _
    _
    (VS_Vowel v, a) xs
    = (v, (Nothing, a)) : xs
  fromFold
    _
    _
    (VS_ImproperDiphthong v, a) xs
    = (improperDiphthongVowel v, (Just S_IotaSubscript, a)) : xs
  fromFold
    _
    c
    (VS_Diphthong d, a) xs
    = consDiphthong c (diphthongVowels d) a xs

  noMarks = Nothing :^ Nothing :^ Nothing

  consDiphthong
    :: DiaeresisConvention
    -> (Vowel, Vowel)
    -> Maybe ContextualAccent :* Maybe Breathing
    -> [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]
    -> [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]

  -- εὐιπποτάτην, εὐιατότερα
  consDiphthong
    (DiaeresisConvention AccentBreaksDiphthong EssentialDiaeresis)
    (v1, v2)
    (a1, b1@(Just _))
    ((v3, (Nothing, a3)) : xs)
    | Just _ <- tryDiphthong v2 v3
    = (v1, noMarks) : (v2 :^ Nothing :^ a1 :^ b1) : (v3, (Nothing, a3)) : xs

  -- Ἁλληλουϊά
  consDiphthong
    (DiaeresisConvention _ EssentialDiaeresis)
    (v1, v2)
    q
    ((v3, (Nothing, a3)) : xs)
    | Just _ <- tryDiphthong v2 v3
    = (v1, noMarks) : (v2 :^ Nothing :^ q) : (v3, (Just S_Diaeresis, a3)) : xs

  -- εὐποιΐας
  consDiphthong
    (DiaeresisConvention _ UselessDiaeresis)
    (v1, v2)
    q
    ((v3, (Nothing, a3)) : xs)
    | isIotaUpsilon v3
    = (v1, noMarks) : (v2 :^ Nothing :^ q) : (v3, (Just S_Diaeresis, a3)) : xs

  consDiphthong
    _
    (v1, v2)
    q
    xs
    = (v1, noMarks) : (v2 :^ Nothing :^ q) : xs

  diphthongVowels :: Diphthong -> (Vowel, Vowel)
  diphthongVowels D_αι = (V_α, V_ι)
  diphthongVowels D_αυ = (V_α, V_υ)
  diphthongVowels D_ει = (V_ε, V_ι)
  diphthongVowels D_ευ = (V_ε, V_υ)
  diphthongVowels D_ηυ = (V_η, V_υ)
  diphthongVowels D_οι = (V_ο, V_ι)
  diphthongVowels D_ου = (V_ο, V_υ)
  diphthongVowels D_υι = (V_υ, V_ι)
  diphthongVowels D_ωι = (V_ω, V_ι)

  improperDiphthongVowel :: ImproperDiphthong -> Vowel
  improperDiphthongVowel I_α = V_α
  improperDiphthongVowel I_η = V_η
  improperDiphthongVowel I_ω = V_ω
