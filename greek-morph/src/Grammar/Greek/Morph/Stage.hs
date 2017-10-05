{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Morph.Stage where

import Grammar.Common
import Grammar.Greek.Morph.Clitic.Stage (clitic)
import Grammar.Greek.Morph.Phoneme.Stage (phoneme)

morph
  = clitic
  <+> phoneme
