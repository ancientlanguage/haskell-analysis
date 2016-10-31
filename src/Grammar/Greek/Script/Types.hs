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

-- this is in expected Unicode order
data Mark = M_Diaeresis | M_Smooth | M_Rough | M_Acute | M_Grave | M_Circumflex | M_IotaSubscript
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

data Final = IsFinal | NotFinal
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

data WordAccent = AW_Acute | AW_Circumflex
  deriving (Eq, Ord, Show, Generic)
instance Serialize WordAccent

data AccentPosition = Ultima | Penult | Antepenult
  deriving (Eq, Ord, Show, Generic)
instance Serialize AccentPosition

data ExtraAccents
  = NoExtraAccents
  | SingleExtraAccent
  deriving (Eq, Ord, Show, Generic)
instance Serialize ExtraAccents

data ForceAcute = NoForceAcute | UseForceAcute
  deriving (Eq, Ord, Show, Generic)
instance Serialize ForceAcute

data Breathing = B_Smooth | B_Rough
  deriving (Eq, Ord, Show, Generic)
instance Serialize Breathing

data SyllabicMark = S_IotaSubscript | S_Diaeresis
  deriving (Eq, Ord, Show, Generic)
instance Serialize SyllabicMark

data ConsonantRho = CR_β | CR_γ | CR_δ | CR_ζ | CR_θ | CR_κ | CR_λ | CR_μ | CR_ν | CR_ξ | CR_π | CR_ρ Breathing | CR_σ | CR_τ | CR_φ | CR_χ | CR_ψ
  deriving (Eq, Ord, Show, Generic)
instance Serialize ConsonantRho

data Diphthong = D_αι | D_αυ | D_ει | D_ευ | D_ηυ | D_οι | D_ου | D_υι | D_ωι
  deriving (Eq, Ord, Show, Generic)
instance Serialize Diphthong

data ImproperDiphthong = I_α | I_η | I_ω
  deriving (Eq, Ord, Show, Generic)
instance Serialize ImproperDiphthong

data VocalicSyllable
  = VS_Vowel Vowel
  | VS_Diphthong Diphthong
  | VS_ImproperDiphthong ImproperDiphthong
  deriving (Eq, Ord, Show, Generic)
instance Serialize VocalicSyllable

data Capitalization = IsCapitalized | NotCapitalized
  deriving (Eq, Ord, Show, Generic)
instance Serialize Capitalization

data Crasis = HasCrasis | NoCrasis
  deriving (Eq, Ord, Show, Generic)
instance Serialize Crasis

data InitialAspiration = HasInitialAspiration | NoInitialAspiration
  deriving (Eq, Ord, Show, Generic)
instance Serialize InitialAspiration

data MarkPreservation = PreserveMarks | Unmarked
  deriving (Eq, Ord, Show, Generic)
instance Serialize MarkPreservation

data HasWordPunctuation = HasWordPunctuation | NoWordPunctuation
  deriving (Eq, Ord, Show, Generic)
instance Serialize HasWordPunctuation
