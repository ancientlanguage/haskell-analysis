{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Forms.Enclitic where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

-- Smyth 181

-- εἰμί pres indic except εἶ
-- φημί pres indic except φῄς
-- the inseparable -δε in ὅδε, τοσόσδε

encliticForms :: [ AccentedWord ]
encliticForms = [accentedWords|
  μοῦ
  μοί
  μέ
  σοῦ
  σοί
  σέ
  οὗ
  οἷ
  ἕ

  τίς
  τί
  τοῦ
  τῷ
  τινός
  τινί

  πού
  ποθί
  πῄ
  ποί
  ποθέν
  ποτέ
  πώ
  πώς

  ἐστί
  ἐστίν
  ἐσμέν
  ἐστέ
  εἰσί
  εἰσίν

  φημί
  φησί
  φαμέν
  φατέ
  φασί
  φασίν

  γέ
  τέ
  τοί
  πέρ
  |]
