{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Conjunctions.Subordinating where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 2770
causal = [accentedWords|
ὅτι
διότι
διόπερ
ἐπεί
ἐπειδή
ὅτε
ὁπότε
ὡς
  |]

comparative = [accentedWords|
ὡς
ὥσπερ
καθάπερ
ὅπως
ᾗ
ὅπῃ
ᾗπερ
  |]

concessive = [accentedWords|
  κεἰ
  κἄν
  |]
-- καὶ εἰ (κεἰ)
-- καὶ ἐόν (κἄν)
-- εἰ καί
-- ἐὰν καί

conditional = [accentedWords|
εἰ
ἐάν
ἤν
ἄν
  |]

consecutive = [accentedWords|
ὥστε
ὡς
  |]

declarative = [accentedWords|
ὅτι
διότι
οὕνεκα
ὁθούνεκα
ὡς
  |]

final = [accentedWords|
ἵνα
ὅπως
ὡς
μή
  |]

local = [accentedWords|
οὗ
ὅπου
οἷ
ὅποι
ἔνθα
ὅθεν
ὁπόθεν
ᾗ
ὅπῃ
  |]

temporal = [accentedWords|
ὅτε
ὁπότε
ἡνίκα
ἐπεί
ἐπειδή
ὡς
μέχρι
ἔστε
ἕως
πρίν
  |]
