{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Morph.Types where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Data
import Data.Serialize (Serialize)
import Grammar.Greek.Script.Types hiding (Case)
import Grammar.Greek.Script.Word

data ShouldElide = ShouldElide | ShouldNotElide
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ShouldElide

data CoreWord = CoreWord
  { coreWordAspiration :: InitialAspiration
  , coreWordSyllables :: [Syllable]
  , coreWordFinalConsonants :: [ConsonantRho]
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize CoreWord

wordToCoreWord :: Word -> CoreWord
wordToCoreWord (Word asp ss fc _ _ _ _ _ _) = CoreWord asp ss fc

data AccentedWord = AccentedWord
  { accentedWordAspiration :: InitialAspiration
  , accentedWordSyllables :: [Syllable]
  , accentedWordFinalConsonants :: [ConsonantRho]
  , accentedWordAccent :: Maybe WordAccent
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize AccentedWord

wordToAccentedWord :: Word -> AccentedWord
wordToAccentedWord (Word asp ss fc a _ _ _ _ _) = AccentedWord asp ss fc a

accentedWordToCoreWord :: AccentedWord -> CoreWord
accentedWordToCoreWord (AccentedWord asp s fc _) = CoreWord asp s fc

data Enclitic = IsEnclitic | NotEnclitic
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Enclitic

data Proclitic = IsProclitic | NotProclitic
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Proclitic

data Case = Nominative | Genitive | Dative | Accusative | Vocative
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Case

data Number = Singular | Dual | Plural
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Number

data Gender = Masculine | Feminine | Neuter
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Gender

data Voice = Active | Middle | Passive
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Voice

data Mood = Indicative | Subjunctive | Optative | Imperative
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Mood

data Tense = Present | Imperfect | Future | Aorist | Perfect | Pluperfect | FuturePerfect
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Tense

data Person = Person1 | Person2 | Person3
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Person

data Morph = Morph
  { morphCase :: Maybe Case
  , morphNumber :: Maybe Number
  , morphGender :: Maybe Gender
  , morphVoice :: Maybe Voice
  , morphMood :: Maybe Mood
  , morphTense :: Maybe Tense
  , morphPerson :: Maybe Person
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Morph

emptyMorph :: Morph
emptyMorph = Morph Nothing Nothing Nothing Nothing Nothing Nothing Nothing

substantiveMorph :: Case -> Number -> Gender -> Morph
substantiveMorph c n g = Morph (Just c) (Just n) (Just g) Nothing Nothing Nothing Nothing

data Phoneme
  = Ph_Aspiration
  | Ph_Consonant ConsonantRho
  | Ph_Vocalic VocalicSyllable
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Phoneme
