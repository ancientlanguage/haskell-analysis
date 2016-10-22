{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Script.Types where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Symbol
  = S_Α | S_Β | S_Γ | S_Δ | S_Ε | S_Ζ | S_Η | S_Θ | S_Ι | S_Κ | S_Λ | S_Μ
  | S_Ν | S_Ξ | S_Ο | S_Π | S_Ρ | S_Σ       | S_Τ | S_Υ | S_Φ | S_Χ | S_Ψ | S_Ω
  | S_α | S_β | S_γ | S_δ | S_ε | S_ζ | S_η | S_θ | S_ι | S_κ | S_λ | S_μ
  | S_ν | S_ξ | S_ο | S_π | S_ρ | S_σ | S_ς | S_τ | S_υ | S_φ | S_χ | S_ψ | S_ω
  deriving (Eq, Ord, Show, Generic)
instance Serialize Symbol 

data Mark = M_Acute | M_Grave | M_Circumflex | M_Smooth | M_Rough | M_IotaSubscript | M_Diaeresis
  deriving (Eq, Ord, Show, Generic)
instance Serialize Mark

data WordPunctuation = P_RightQuote
  deriving (Eq, Ord, Show, Generic)
instance Serialize WordPunctuation

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show, Generic)
instance Serialize Letter

data Case = Lowercase | Uppercase
  deriving (Eq, Ord, Show, Generic)
instance Serialize Case

data Final = FinalNotSupported | IsFinal | IsNotFinal
  deriving (Eq, Ord, Show, Generic)
instance Serialize Final

data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
  deriving (Eq, Ord, Show, Generic)
instance Serialize Vowel

data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ
  deriving (Eq, Ord, Show, Generic)
instance Serialize Consonant

data EndOfSentence = IsEndOfSentence | NotEndOfSentence
  deriving (Eq, Ord, Show, Generic)
instance Serialize EndOfSentence

data Elision = IsElided | NotElided
  deriving (Eq, Ord, Show, Generic)
instance Serialize Elision

data Accent = A_Acute | A_Grave | A_Circumflex
  deriving (Eq, Ord, Show, Generic)
instance Serialize Accent

data Breathing = B_Smooth | B_Rough
  deriving (Eq, Ord, Show, Generic)
instance Serialize Breathing

data SyllabicMark = S_IotaSubscript | S_Diaeresis
  deriving (Eq, Ord, Show, Generic)
instance Serialize SyllabicMark

data Diphthong = D_αι | D_αυ | D_ει | D_ευ | D_ηυ | D_οι | D_ου | D_υι
  deriving (Eq, Ord, Show, Generic)
instance Serialize Diphthong

data ImproperDiphthong = I_α | I_η | I_ω
  deriving (Eq, Ord, Show, Generic)
instance Serialize ImproperDiphthong

data Capitalized = IsCapitalized | IsNotCapitalized
  deriving (Eq, Ord, Show, Generic)
instance Serialize Capitalized

data Crasis = HasCrasis | NoCrasis
  deriving (Eq, Ord, Show, Generic)
instance Serialize Crasis

data SentenceBoundary = NotSentenceEnd | SentenceEnd
  deriving (Eq, Ord, Show, Generic)
instance Serialize SentenceBoundary
