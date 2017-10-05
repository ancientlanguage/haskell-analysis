{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Grammar.Greek.Script.Word where

import Prelude hiding (Word)
import Control.Lens (makeLensesFor)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Script.Types

data WordAccent = WordAccent
  { accentValue :: BasicAccent
  , accentPosition :: AccentPosition
  , accentForce :: ForceAcute
  , accentExtra :: ExtraAccents
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordAccent
makeLensesFor
  [ ("accentValue", "_accentValue")
  , ("accentPosition", "_accentPosition")
  , ("accentForce", "_accentForce")
  , ("accentExtra", "_accentExtra")
  ]
  ''WordAccent

data Syllable = Syllable
  { syllableConsonants :: [ConsonantRho]
  , syllableVowel :: VocalicSyllable
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Syllable
makeLensesFor
  [ ("syllableConsonants", "_syllableConsonants")
  , ("syllableVowel", "_syllableVowel")
  ]
  ''Syllable

data Word = Word
  { wordInitialAspiration :: InitialAspiration
  , wordSyllables :: [Syllable]
  , wordFinalConsonants :: [ConsonantRho]
  , wordAccent :: Maybe WordAccent
  , wordCrasis :: Crasis
  , wordElision :: Elision
  , wordMarkPreservation :: MarkPreservation
  , wordDiaeresisConvention :: DiaeresisConvention
  , wordCapitalization :: Capitalization
  , wordPunctuation :: HasWordPunctuation
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Word
makeLensesFor
  [ ("wordInitialAspiration", "_wordInitialAspiration")
  , ("wordSyllables", "_wordSyllables")
  , ("wordFinalConsonants", "_wordFinalConsonants")
  , ("wordAccent", "_wordAccent")
  , ("wordCrasis", "_wordCrasis")
  , ("wordElision", "_wordElision")
  , ("wordMarkPreservation", "_wordMarkPreservation")
  , ("wordDiaeresisConvention", "_wordDiaeresisConvention")
  , ("wordCapitalization", "_wordCapitalization")
  , ("wordPunctuation", "_wordPunctuation")
  ]
  ''Word
