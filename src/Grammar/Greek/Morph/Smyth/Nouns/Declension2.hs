{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Nouns.Declension2 where

import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 230
declension2Masculine = makeParadigmForm
  (NounFormKind Declension2 NotContract)
  (genderMorph Masculine)
  $ concat
  [ [nounParadigm|
ἵππο-ς
ἵππου
ἵππῳ
ἵππο-ν
ἵππε

ἵππω
ἵπποιν

ἵπποι
ἵππων
ἵπποις
ἵππους
    |]
  , [nounParadigm|
ἄνθρωπο-ς
ἀνθρώπου
ἀνθρώπῳ
ἄνθρωπο-ν
ἄνθρωπε

ἀνθρώπω
ἀνθρώποιν

ἄνθρωποι
ἀνθρώπων
ἀνθρώποις
ἀνθρώπους
    |]
  ]

declension2Feminine = makeParadigmForm
  (NounFormKind Declension2 NotContract)
  (genderMorph Feminine)
  $ concat
  [ [nounParadigm|
ὁδό-ς
ὁδοῦ
ὁδῷ
ὁδό-ν
ὁδέ

ὁδώ
ὁδοῖν

ὁδοί
ὁδῶν
ὁδοῖς
ὁδούς
    |]
  ]

declension2Neuter = makeParadigmForm
  (NounFormKind Declension2 NotContract)
  (genderMorph Neuter)
  $ concat
  [ [nounParadigm|
δῶρο-ν
δώρου
δώρῳ
δῶρο-ν
δῶρο-ν

δώρω
δώροιν

δῶρα
δώρων
δώροις
δῶρα
    |]
  ]

-- Smyth 235. Contracts
declension2ContractMasculine = makeParadigmForm
  (NounFormKind Declension2 IsContract)
  (genderMorph Masculine)
  $ concat
  [ [nounParadigm|
νοῦ
νῷ
νοῦ-ν
νοῦ
  *

νώ
νοῖν

νοῖ
νῶν
νοῖς
νοῦς
    |]
    , [nounParadigm|
περίπλου
περίπλῳ
περίπλου-ν
περίπλου
  *

περίπλω
περίπλοιν

περίπλοι
περίπλων
περίπλοις
περίπλους
    |]
  ]

declension2ContractNeuter = makeParadigmForm
  (NounFormKind Declension2 IsContract)
  (genderMorph Neuter)
  $ concat
  [ [nounParadigm|
ὀστοῦ
ὀστῷ
ὀστοῦ-ν
ὀστοῦ-ν
  *

ὀστώ
ὀστοῖν

ὀστᾶ
ὀστῶν
ὀστοῖς
ὀστᾶ
    |]
  ]

declension2Forms :: [ParadigmForm]
declension2Forms
  = declension2Masculine
  ++ declension2Feminine
  ++ declension2Neuter
  ++ declension2ContractMasculine
  ++ declension2ContractNeuter
