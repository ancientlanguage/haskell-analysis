{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Clitic.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data WordClitic = WordClitic
  CoreWord
  (Maybe WordAccent)
  Enclitic
  Proclitic
  Crasis
  ShouldElide
  MarkPreservation
  Capitalization
  HasWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordClitic
