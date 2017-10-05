{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Grammar.Greek.Script.Types where

import Data.Data
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Symbol
  = S_Α | S_Β | S_Γ | S_Δ | S_Ε | S_Ζ | S_Η | S_Θ | S_Ι | S_Κ | S_Λ | S_Μ
  | S_Ν | S_Ξ | S_Ο | S_Π | S_Ρ | S_Σ       | S_Τ | S_Υ | S_Φ | S_Χ | S_Ψ | S_Ω
  | S_α | S_β | S_γ | S_δ | S_ε | S_ζ | S_η | S_θ | S_ι | S_κ | S_λ | S_μ
  | S_ν | S_ξ | S_ο | S_π | S_ρ | S_σ | S_ς | S_τ | S_υ | S_φ | S_χ | S_ψ | S_ω
  | S_ϝ
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Symbol

-- this is in expected Unicode order
data Mark = M_Diaeresis | M_Smooth | M_Rough | M_Acute | M_Grave | M_Circumflex | M_IotaSubscript
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Mark

data WordPunctuation = P_RightQuote
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize WordPunctuation

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ
  | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  | L_ϝ
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Letter

data Case = Lowercase | Uppercase
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Case

data Final = IsFinal | NotFinal
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Final

data Vowel = V_α | V_ε | V_η | V_ι | V_ο | V_υ | V_ω
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Vowel

data Consonant = C_β | C_γ | C_δ | C_ζ | C_θ | C_κ | C_λ | C_μ | C_ν | C_ξ | C_π | C_ρ | C_σ | C_τ | C_φ | C_χ | C_ψ | C_ϝ
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Consonant

data EndOfSentence = IsEndOfSentence | NotEndOfSentence
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize EndOfSentence

-- For Aphaeresis (Inverse Elision), see Smyth 76
data Elision = IsElided | Aphaeresis | NotElided
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Elision

data ContextualAccent = AC_Acute | AC_Grave | AC_Circumflex
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ContextualAccent

data BasicAccent = AB_Acute | AB_Circumflex
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize BasicAccent

data AccentPosition = Ultima | Penult | Antepenult | Preantepenult
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize AccentPosition

data ExtraAccents
  = NoExtraAccents
  | ExtraAcuteUltima
  | ExtraAcutePenult
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ExtraAccents

data ForceAcute = NoForceAcute | DoForceAcute
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ForceAcute

data Breathing = B_Smooth | B_Rough
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Breathing

data SyllabicMark = S_IotaSubscript | S_Diaeresis
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize SyllabicMark

data ConsonantRho = CR_β | CR_γ | CR_δ | CR_ζ | CR_θ | CR_κ | CR_λ | CR_μ | CR_ν | CR_ξ | CR_π | CR_ρ Breathing | CR_σ | CR_τ | CR_φ | CR_χ | CR_ψ | CR_ϝ
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ConsonantRho

data Diphthong = D_αι | D_αυ | D_ει | D_ευ | D_ηυ | D_οι | D_ου | D_υι | D_ωι
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Diphthong

data ImproperDiphthong = I_α | I_η | I_ω
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize ImproperDiphthong

data AccentBreaksDiphthong = AccentBreaksDiphthong | AccentNotBreaksDiphthong
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize AccentBreaksDiphthong

data UselessDiaeresis = UselessDiaeresis | EssentialDiaeresis
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize UselessDiaeresis

data DiaeresisConvention = DiaeresisConvention
  { diaeresisConventionAccent :: AccentBreaksDiphthong
  , diaeresisConventionUseless :: UselessDiaeresis
  }
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize DiaeresisConvention

basicDiaeresisConvention :: DiaeresisConvention
basicDiaeresisConvention = DiaeresisConvention AccentNotBreaksDiphthong EssentialDiaeresis

data VocalicSyllable
  = VS_Vowel Vowel
  | VS_Diphthong Diphthong
  | VS_ImproperDiphthong ImproperDiphthong
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize VocalicSyllable

data Capitalization = IsCapitalized | NotCapitalized
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Capitalization

data Crasis = HasCrasis Int | NoCrasis
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize Crasis

data InitialAspiration = HasInitialAspiration | NoInitialAspiration
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize InitialAspiration

data MarkPreservation = PreserveMarks | Unmarked
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize MarkPreservation

data HasWordPunctuation = HasWordPunctuation | NoWordPunctuation
  deriving (Eq, Ord, Show, Generic, Data, Typeable)
instance Serialize HasWordPunctuation
