{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Phoneme.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data WordPhoneme = WordPhoneme
  [Phoneme]
  (Maybe WordAccent)
  Enclitic
  Proclitic
  Crasis
  Elision
  MarkPreservation
  DiaeresisConvention
  Capitalization
  HasWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordPhoneme
