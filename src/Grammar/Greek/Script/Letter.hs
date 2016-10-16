module Grammar.Greek.Script.Letter where

data Letter
  = L_α | L_β | L_γ | L_δ | L_ε | L_ζ | L_η | L_θ | L_ι | L_κ | L_λ | L_μ | L_ν | L_ξ | L_ο | L_π | L_ρ | L_σ | L_τ | L_υ | L_φ | L_χ | L_ψ | L_ω
  deriving (Eq, Ord, Show)
data ConcreteLetter
  = CL_Α | CL_Β | CL_Γ | CL_Δ | CL_Ε | CL_Ζ | CL_Η | CL_Θ | CL_Ι | CL_Κ | CL_Λ | CL_Μ | CL_Ν | CL_Ξ | CL_Ο | CL_Π | CL_Ρ | CL_Σ | CL_Τ | CL_Υ | CL_Φ | CL_Χ | CL_Ψ | CL_Ω
  | CL_α | CL_β | CL_γ | CL_δ | CL_ε | CL_ζ | CL_η | CL_θ | CL_ι | CL_κ | CL_λ | CL_μ | CL_ν | CL_ξ | CL_ο | CL_π | CL_ρ | CL_σ | CL_ς | CL_τ | CL_υ | CL_φ | CL_χ | CL_ψ | CL_ω
  deriving (Eq, Ord, Show)

data Case = Uppercase | Lowercase
  deriving (Eq, Ord, Show)
data Final = IsFinal | NotFinal | NAFinal
  deriving (Eq, Ord, Show)

data Acute = Acute deriving (Eq, Ord, Show)
data Grave = Grave deriving (Eq, Ord, Show)
data Circumflex = Circumflex deriving (Eq, Ord, Show)
data Smooth = Smooth deriving (Eq, Ord, Show)
data Rough = Rough deriving (Eq, Ord, Show)
data Diaeresis = Diaeresis deriving (Eq, Ord, Show)
data IotaSubscript = IotaSubscript deriving (Eq, Ord, Show)
data RightQuote = RightQuote deriving (Eq, Ord, Show)
