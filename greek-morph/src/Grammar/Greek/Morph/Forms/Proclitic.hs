{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Forms.Proclitic where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

-- Smyth 179

procliticForms :: [ CoreWord ]
procliticForms = [coreWords|
  ὁ
  ἡ
  οἱ
  αἱ
  ἐν
  εἰς
  ἐς
  ἐξ
  ἐκ
  εἰ
  ὡς
  οὐ
  οὐκ
  οὐχ
  |]
