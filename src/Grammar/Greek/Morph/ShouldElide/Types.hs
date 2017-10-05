{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.ShouldElide.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data WordShouldElide = WordShouldElide
  CoreWord
  (Maybe WordAccent)
  Crasis
  ShouldElide
  MarkPreservation
  DiaeresisConvention
  Capitalization
  HasWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordShouldElide

wordShouldElideCoreWord :: WordShouldElide -> CoreWord
wordShouldElideCoreWord (WordShouldElide x _ _ _ _ _ _ _) = x
wordShouldElideShouldElide :: WordShouldElide -> ShouldElide
wordShouldElideShouldElide (WordShouldElide _ _ _ x _ _ _ _) = x
