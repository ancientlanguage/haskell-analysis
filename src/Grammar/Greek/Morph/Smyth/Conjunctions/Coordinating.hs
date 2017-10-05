{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Conjunctions.Coordinating where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 2163
copulative = [accentedWords|
τέ
καί
οὐδέ
μηδέ
οὔτε
μήτε
  |]

adversative = [accentedWords|
ἀλλά
δέ
μέν
ἀτάρ
μέντοι
καίτοι
  |]

disjunctive = [accentedWords|
ἤ
εἴτε
  |]

inferential = [accentedWords|
ἄρα
οὖν
νῦν
τοίνυν
τοιγάρ
τοιγάρτοι
τοιγαροῦν
  |]

causal = [accentedWords|
γάρ
  |]
