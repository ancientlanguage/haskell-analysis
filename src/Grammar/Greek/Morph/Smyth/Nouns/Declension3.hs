{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Declension3 where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 256. STEMS IN A LABIAL (π, β, φ)  OR IN A PALATAL (κ, γ, χ)
declension3LabialPalatal =
  [ [nounParadigm|
Αἰθίοψ
Αἰθίοπ-ος
Αἰθίοπ-ι
Αἰθίοπ-α
Αἰθίοψ

Αἰθίοπ-ε
Αἰθιόπ-οιν

Αἰθίοπ-ες
Αἰθιόπ-ων
Αἰθίοψι(ν)
Αἰθίοπ-ας
    |]
  , [nounParadigm|
φλέψ
φλεβ-ός
φλεβ-ί
φλέβ-α
φλέψ

φλέβ-ε
φλεβ-οῖν

φλέβ-ες
φλεβ-ῶν
φλεψί(ν)
φλέβ-ας
    |]
  , [nounParadigm|
φύλαξ
φύλακ-ος
φύλακ-ι
φύλακ-α
φύλαξ

φύλακ-ε
φυλάκ-οιν

φύλακ-ες
φυλάκ-ων
φύλαξι(ν)
φύλακ-ας
    |]
  , [nounParadigm|
φάλαγξ
φάλαγγ-ος
φάλαγγ-ι
φάλαγγ-α
φάλαγξ

φάλαγγ-ε
φαλάγγ-οιν

φάλαγγ-ες
φαλάγγ-ων
φάλαγξι(ν)
φάλαγγ-ας
    |]
  , [nounParadigm|
αἴξ
αἰγ-ός
αἰγ-ί
αἶγ-α
αἴξ

αἶγ-ε
αἰγ-οῖν

αἶγ-ες
αἰγ-ῶν
αἰξί(ν)
αἶγ-ας
    |]
  , [nounParadigm|
θρίξ
θρίξ
τριχ-ός
τριχ-ί
τρίχ-α

τρίχ-ε
τριχ-οῖν

τρίχ-ες
τριχ-ῶν
θριξί(ν)
τρίχ-ας
    |]
  ]

-- Smyth 257. STEMS IN A DENTAL (τ, δ, θ)
declension3DentalMasculineFeminine =
  [ [nounParadigm|
θής
θητ-ός
θητ-ί
θῆτ-α
θής

θῆτ-ε
θητ-οῖν

θῆτ-ες
θητ-ῶν
θησί(ν)
θῆτ-ας
    |]
    , [nounParadigm|
ἐλπίς
ἐλπίδ-ος
ἐλπίδ-ι
ἐλπίδ-α
ἐλπί

ἐλπίδ-ε
ἐλπίδ-οιν

ἐλπίδ-ες
ἐλπίδ-ων
ἐλπίσι(ν)
ἐλπίδ-ας
    |]
    , [nounParadigm|
χάρις
χάριτ-ος
χάριτ-ι
χάριν
χάρι

χάριτ-ε
χαρίτ-οιν

χάριτ-ες
χαρίτ-ων
χάρισι(ν)
χάριτ-ας
    |]
    , [nounParadigm|
ὄρνῑς
ὄρνῑθ-ος
ὄρνῑθ-ι
ὄρνῑν
ὄρνῑ

ὄρνῑθ-ε
ὀρνί̄θ-οιν

ὄρνῑθ-ες
ὀρνί̄θ-ων
ὄρνῑσι(ν)
ὄρνῑθ-ας
    |]
    , [nounParadigm|
γίγᾱς
γίγαντ-ος
γίγαντ-ι
γίγαντ-α
γίγαν

γίγαντ-ε
γιγάντ-οιν

γίγαντ-ες
γιγάντ-ων
γίγᾱσι(ν)
γίγαντ-ας
    |]
    , [nounParadigm|
γέρων
γέροντ-ος
γέροντ-ι
γέροντ-α
γέρον

γέροντ-ε
γερόντ-οιν

γέροντ-ες
γερόντ-ων
γέρουσι(ν)
γέροντ-ας
    |]
  ]

  -- Smyth 258. NEUTERS WITH STEMS IN τ AND IN ᾱτ VARYING WITH ας AND STEMS IN τ AND IN ᾱτ VARYING WITH ας
declension3Neuter =
  [ [nounParadigm|
σῶμα
σώματ-ος
σώματ-ι
σῶμα
σῶμα

σώματ-ε
σωμάτ-οιν

σώματ-α
σωμάτ-ων
σώμασι(ν)
σώματ-α
    |]
   , [nounParadigm|
ἧπαρ
ἥπατ-ος
ἥπατ-ι
ἧπαρ
ἧπαρ

ἧπαρ
ἥπατ-ος

ἥπατ-α
ἡπάτ-ων
ἥπασι(ν)
ἥπατ-α
    |]  
  , [nounParadigm|
τέρας
τέρατ-ος
τέρατ-ι
τέρας
τέρας

τέρας
τέρατ-ος

τέρατ-α
τεράτ-ων
τέρασι(ν)
τέρατ-α
  |]  
  , [nounParadigm|
κέρας
κέρᾱτ-ος
κέρᾱτ-ι
κέρας
κέρας

κέρας
κέρᾱτ-ος

κέρᾱτ-α
κερά̄τ-ων
κέρᾱσι(ν)
κέρᾱτ-α
  |]
  , [nounParadigm|
*
κέρως
κέραι
*
*

*
κέρως

κέρᾱ
κερῶν
*
κέρᾱ
    |]
  ]

 -- Smyth 259. STEMS IN A LIQUID (λ, ρ) OR A NASAL (ν)
declension3LiquidNasal =
  [ [nounParadigm|
θήρ
θηρ-ός
θηρ-ί
θηρ-α
θήρ

θῆρ-ε
θηρ-οῖν

θῆρ-ες
θηρ-ῶν
θηρ-σί(ν)
θῆρ-ας
  |]
  , [nounParadigm|
ῥήτωρ
ῥήτορ-ος
ῥήτορ-ι
ῥήτορ-α
ῥῆτορ

ῥήτορ-ε
ῥητόρ-οιν

ῥήτορ-ες
ῥητόρ-ων
ῥήτορ-σι(ν)
ῥήτορ-ας
  |]
  , [nounParadigm|
ῥῑ́ς
ῥῑν-ός
ῥῑν-ί
ῥῖν-α
ῥῑ́ς

ῥῖν-ε
ῥῑν-οῖν

ῥῖν-ες
ῥῑν-ῶν
ῥῑσί(ν)
ῥῖν-ας
  |]
  , [nounParadigm|
ἡγεμών
ἡγεμόν-ος
ἡγεμόν-ι
ἡγεμόν-α
ἡγεμών

ἡγεμόν-ε
ἡγεμόν-οιν

ἡγεμόν-ες
ἡγεμόν-ων
ἡγεμόσι(ν)
ἡγεμόν-ας
  |]
  , [nounParadigm|
ἀγών
ἀγῶν-ος
ἀγῶν-ι
ἀγῶν-α
ἀγών

ἁγῶν-ε
ἀγών-οιν

ἀγῶν-ες
ἀγών-ων
ἀγῶσι(ν)
ἀγῶν-ας
  |]
  , [nounParadigm|
ποιμήν
ποιμέν-ος
ποιμέν-ι
ποιμέν-α
ποιμήν

ποιμέν-ε
ποιμέν-οιν

ποιμέν-ες
ποιμέν-ων
ποιμέσι(ν)
ποιμέν-ας
    |]
  ]

-- Smyth 262. STEMS IN ερ VARYING WITH ρ
declension3StemερWithρ=
  [ [nounParadigm|
πατήρ
πατρ-ός
πατρ-ί
πατέρ-α
πάτερ

πατέρ-ε
πατέρ-οιν

πατέρ-ες
πατέρ-ων
πατρά-σι(ν)
πατέρ-ας
    |]
  , [nounParadigm|
μήτηρ
μητρ-ός
μητρ-ί
μητέρ-α
μῆτερ

μητέρ-ε
μητέρ-οιν

μητέρ-ες
μητέρ-ων
μητρά-σι(ν)
μητέρ-ας
  |]
  , [nounParadigm|
θυγάτηρ
θυγατρ-ός
θυγατρ-ί
θυγατέρ-α
θύγατερ

θυγατέρ-ε
θυγατέρ-οιν

θυγατέρ-ες
θυγατέρ-ων
θυγατρά-σι(ν)
θυγατέρ-ας
  |]
  , [nounParadigm|
ἀνήρ
ἀνδρ-ός
ἀνδρ-ί
ἄνδρ-α
ἄνερ

ἄνδρ-ε
ἀνδρ-οῖν

ἄνδρ-ες
ἀνδρ-ῶν
ἀνδρά-σι(ν)
ἄνδρ-ας
    |]
  ]

-- Smyth 263. STEMS Ending IN SIGMA (ες, ας, ος)
declension3StemSigma=
  [ [nounParadigm|
τριήρης
τριήρους
τριήρει
τριήρη
τριῆρες

τριήρει
τριήροιν

τριήρεις
τριήρων
τριήρεσι(ν)
τριῄρεις
  |]
  , [nounParadigm|
γένος
γένους
γένει
γένος
γένος

γένει
γενοῖν

γένη
γενῶν
γένεσι
γένη
  |]
  , [nounParadigm|
γέρας
γέρως
γέραι
γέρας
γέρας

γέρᾱ
γερῷν

γέρᾱ
γερῶν
γέρασι(ν)
γέρᾱ
    |]
  ]
-- Smyth 268. STEMS IN ι AND υ
declension3StemIotaAndUpsilon=
  [ [nounParadigm|
πόλι-ς
πόλε-ως
πόλει
πόλι-ν
πόλι

πόλει
πολέ-οιν

πόλεις
πόλε-ων
πόλε-σι(ν)
πόλεις
  |]
  , [nounParadigm|
πῆχυ-ς
πήχε-ως
πήχει
πῆχυ-ν
πῆχυ

πήχει
πηχέ-οιν

πήχεις
πήχε-ων
πήχε-σι(ν)
πήχεις
  |]
  , [nounParadigm|
ἄστυ
ἄστε-ως
ἄστει
ἄστυ
ἄστυ

ἄστει
ἀστέ-οιν

ἄστη
ἄστε-ων
ἄστε-σι(ν)
ἄστη
  |]
  , [nounParadigm|
σῦ-ς
συ-ός
συ-ῑ́
σῦ-ν
σῦ

σύ-ε
σν-οῖν

σύ-ες
συ-ῶν
συ-σί(ν)
σῦς
  |]
  , [nounParadigm|
ἰχθῡ́-ς
ιχθύ-ος
ἰχθύ-ϊ
ἰχθῡ́-ν
ἰχθῡ́

ἰχθύ-ε
ἰχθύ-οιύ

ἰχθύ-ες
ἰχθύ-ων
ἰχθύ-σι(ν)
ἰχθῦς
    |]
  ]
