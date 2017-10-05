{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Declension3 where

import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.QuasiQuoters

decl3 g ps = makeParadigmForm
  (NounFormKind Declension3 NotContract)
  (genderMorph g)
  (concat ps)

-- Smyth 256. STEMS IN A LABIAL (π, β, φ)  OR IN A PALATAL (κ, γ, χ)
declension3LabialPalatalMasculine = decl3 Masculine
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
  ]

declension3LabialPalatalFeminine = decl3 Feminine
  [ [nounParadigm|
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
*
*
τριχ-ός
τριχ-ί
τρίχ-α

τρίχ-ε
τριχ-οῖν

τρίχ-ες
τριχ-ῶν
*
τρίχ-ας
    |]
  , [nounParadigm|
θρίξ
θρίξ
*
*
*

*
*

*
*
θριξί(ν)
*
    |]
  ]

-- Smyth 257. STEMS IN A DENTAL (τ, δ, θ)
declension3DentalMasculine = decl3 Masculine
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
  ]

declension3DentalFeminine = decl3 Feminine
    [ [nounParadigm|
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
  ]

  -- Smyth 258. NEUTERS WITH STEMS IN τ AND IN ᾱτ VARYING WITH ας AND STEMS IN τ AND IN ᾱτ VARYING WITH ας
declension3Neuterτ = decl3 Neuter
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

ἧπατ-ε
ἥπατ-οιν

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

τέρατ-ε
τέρατ-οιν

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

κέρᾱτ-ε
κέρᾱτ-οιν

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

κέρᾱ
κέρως

κέρᾱ
κερῶν
*
κέρᾱ
    |]
  ]

 -- Smyth 259. STEMS IN A LIQUID (λ, ρ) OR A NASAL (ν)
declension3LiquidNasalMasculine = decl3 Masculine
  [ [nounParadigm|
θήρ
θηρ-ός
θηρ-ί
θῆρ-α
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

ἀγῶν-ε
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

declension3LiquidNasalFeminine = decl3 Feminine
  [ [nounParadigm|
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
  ]

-- Smyth 262. STEMS IN ερ VARYING WITH ρ
declension3StemερWithρMasculine = decl3 Masculine
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

declension3StemερWithρFeminine = decl3 Feminine
  [ [nounParadigm|
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
  ]

-- Socrates, Demosthenes

-- Smyth 263. STEMS Ending IN SIGMA (ες, ας, ος)
declension3StemSigmaFeminine = decl3 Feminine
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
  ]

declension3StemSigmaNeuter = decl3 Neuter
  [ [nounParadigm|
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

-- 265 deos
-- 267 heros

-- Smyth 268. STEMS IN ι AND υ
declension3StemιAndυFeminine = decl3 Feminine
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
  ]

declension3StemιAndυMasculine = decl3 Masculine
  [ [nounParadigm|
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

declension3StemιAndυNeuter = decl3 Neuter
  [ [nounParadigm|
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
  ]

-- Smyth 275. STEMS IN ευ, αυ, ου
declension3StemευAndαυAndουMasculine = decl3 Masculine
  [ [nounParadigm|
βασιλεύ-ς
βασιλέ-ως
βασιλεῖ
βασιλέ-ᾱ
βασιλεῦ

βασιλῆ
βασιλέ-οιν

βασιλῆς
βασιλέ-ων
βασιλεῦ-σι(ν)
βασιλέ-ᾱς
    |]
    , [nounParadigm|
βοῦ-ς
βο-ός
βο-ῑ́
βοῦ-ν
βοῦ

βό-ε
βο-οῖν

βό-ες
βο-ῶν
βου-σί(ν)
βοῦ-ς
    |]
  ]

declension3StemευAndαυAndουFeminine = decl3 Feminine
  [ [nounParadigm|
γραῦ-ς
γρᾱ-ός
γρᾱ-ῑ́
γραῦ-ν
γραῦ

γρᾶ-ε
γρᾱ-οῖν

γρᾶ-ες
γρᾱ-ῶν
γραυ-σί(ν)
γραῦ-ς
    |]
  , [nounParadigm|
ναῦ-ς
νε-ώς
νη-ῑ́
ναῦ-ν
ναῦ

νῆ-ε
νε-οῖν

νῆ-ες
νε-ῶν
ναυ-σί(ν)
ναῦ-ς
    |]
  , [nounParadigm|
βοῦ-ς
βο-ός
βο-ῑ́
βοῦ-ν
βοῦ

βό-ε
βο-οῖν

βό-ες
βο-ῶν
βου-σί(ν)
βοῦ-ς
    |]
  ]

declension3Forms :: [ParadigmForm]
declension3Forms
  = declension3LabialPalatalMasculine
  ++ declension3LabialPalatalFeminine
  ++ declension3DentalMasculine
  ++ declension3DentalFeminine
  ++ declension3Neuterτ
  ++ declension3LiquidNasalMasculine
  ++ declension3LiquidNasalFeminine
  ++ declension3StemερWithρMasculine
  ++ declension3StemερWithρFeminine
  ++ declension3StemSigmaFeminine
  ++ declension3StemSigmaNeuter
  ++ declension3StemιAndυFeminine
  ++ declension3StemιAndυMasculine
  ++ declension3StemιAndυNeuter
  ++ declension3StemευAndαυAndουMasculine
  ++ declension3StemευAndαυAndουFeminine
