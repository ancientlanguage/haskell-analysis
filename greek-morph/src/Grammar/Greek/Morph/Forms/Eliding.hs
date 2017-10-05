{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Forms.Eliding where

import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types

elidingForms :: [ CoreWord ]
elidingForms = [coreWords|
  ἀντί
  ἀπό
  διά
  ἐπί
  κατά
  μετά
  παρά
  ὑπό

  ἀλλά

  δέ
  μηδέ
  οὐδέ

  τοῦτο
  |]
