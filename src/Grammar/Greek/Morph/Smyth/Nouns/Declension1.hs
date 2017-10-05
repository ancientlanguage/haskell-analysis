{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Declension1 where

import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 216. Feminines
declension1Feminines = makeParadigmForm
  (NounFormKind Declension1 NotContract)
  (genderMorph Feminine)
  $ concat
  [ [nounParadigm|
χώρᾱ
χώρᾱς
χώρᾳ
χώρᾱ-ν
χώρᾱ

χώρᾱ
χώραιν

χῶραι
χωρῶν
χώραις
χώρᾱς
    |]
  , [nounParadigm|
νί̄κη
νί̄κης
νί̄κῃ
νί̄κη-ν
νί̄κη

νί̄κᾱ
νί̄καιν

νῖκαι
νῑκῶν
νί̄καις
νί̄κᾱς
    |]
  , [nounParadigm|
φυγή
φυγῆς
φυγῇ
φυγή-ν
φυγή

φυγά̄
φυγαῖν

φυγαί
φυγῶν
φυγαῖς
φυγά̄ς
    |]
  , [nounParadigm|
μοῖρα
μοίρᾱς
μοίρᾳ
μοῖρα-ν
μοῖρα

μοίρᾱ
μοίραιν

μοῖραι
μοιρῶν
μοίραις
μοίρᾱς
    |]
  , [nounParadigm|
γλῶττα
γλώττης
γλῶττῃ
γλῶττα-ν
γλῶττα

γλώττᾱ
γλώτταιν

γλῶτται
γλωττῶν
γλώτταις
γλώττᾱς
    |]
  , [nounParadigm|
θάλαττα
θαλάττης
θαλάττῃ
θάλαττα-ν
θάλαττα

θαλάττᾱ
θαλάτταιν

θάλατται
θαλαττῶν
θαλάτταις
θαλάττᾱς
    |]
  ]

-- Smyth 222
declension1Masculines = makeParadigmForm
  (NounFormKind Declension1 NotContract)
  (genderMorph Masculine)
  $ concat
  [ [nounParadigm|
νεᾱνίᾱ-ς
νεᾱνίου
νεᾱνίᾱͅ
νεᾱνίᾱ-ν
νεᾱνίᾱ

νεᾱνίᾱ
νεᾱνίαιν

νεᾱνίαι
νεᾱνιῶν
νεᾱνίαις
νεᾱνίᾱς
    |]
  , [nounParadigm|
πολί̄της
πολί̄του
πολί̄τῃ
πολί̄τη-ν
πολῖτα

πολί̄τᾱ
πολί̄ταιν

πολῖται
πολῑτῶν
πολί̄ταις
πολί̄τᾱς
    |]
  , [nounParadigm|
κριτής
κριτοῦ
κριτῇ
κριτή-ν
κριτά

κριτά̄
κριταῖν

κριταί
κριτῶν
κριταῖς
κριτά̄ς
    |]
  , [nounParadigm|
Ἀτρείδης
Ἀτρείδου
Ἀτρείδῃ
Ἀτρείδη-ν
Ἀτρείδη

Ἀτρείδᾱ
Ἀτρείδαιν

Ἀτρεῖδαι
Ἀτρειδῶν
Ἀτρείδαις
Ἀτρείδᾱς
    |]
  ]

-- Smyth 227. Contracts
declension1ContractsFeminine = makeParadigmForm
  (NounFormKind Declension1 IsContract)
  (genderMorph Feminine)
  $ concat
  [ [nounParadigm|
μνᾶ
μνᾶς
μνᾷ
μνᾶ-ν
μνᾶ

μνᾶ
μναῖν

μναῖ
μνῶν
μναῖς
μνᾶς
    |]
  , [nounParadigm|
σῡκῆ
σῡκῆς
σῡκῇ
σῡκῆν
σῡκῆ

σῡκᾶ
σῡκαῖν

σῡκαῖ
σῡκῶν
σῡκαῖς
σῡκᾶς
    |]
  ]

declension1ContractsMasculine = makeParadigmForm
  (NounFormKind Declension1 IsContract)
  (genderMorph Masculine)
  $ concat
  [ [nounParadigm|
Βορρᾶ-ς
Βορροῦ
Βορρᾷ
Βορρᾶν
Βορρᾶ

*
*

*
*
*
*
    |]
  , [nounParadigm|
Ἑρμῆ-ς
Ἑρμοῦ
Ἑρμῆ
Ἑρμῆ-ν
Ἑρμῆ

Ἑρμᾶ
Ἑρμαῖν

Ἑρμαῖ
Ἑρμῶν
Ἑρμαῖς
Ἑρμᾶς
    |]
  ]

declension1Forms :: [ParadigmForm]
declension1Forms
  = declension1Feminines
  ++ declension1Masculines
  ++ declension1ContractsFeminine
  ++ declension1ContractsMasculine
