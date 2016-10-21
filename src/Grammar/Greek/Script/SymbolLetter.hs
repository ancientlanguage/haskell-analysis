module Grammar.Greek.Script.SymbolLetter where

import Control.Lens (over)
import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data Error = InvalidCombo Letter Case Final 
  deriving (Show)

symbolLetter :: Around Void Error Symbol (Letter :* Case :* Final)
symbolLetter = Around (Success . to) (over _Failure pure . from)
  where
  to S_Α = (L_α, (Uppercase, FinalNotSupported))
  to S_Β = (L_β, (Uppercase, FinalNotSupported))
  to S_Γ = (L_γ, (Uppercase, FinalNotSupported))
  to S_Δ = (L_δ, (Uppercase, FinalNotSupported))
  to S_Ε = (L_ε, (Uppercase, FinalNotSupported))
  to S_Ζ = (L_ζ, (Uppercase, FinalNotSupported))
  to S_Η = (L_η, (Uppercase, FinalNotSupported))
  to S_Θ = (L_θ, (Uppercase, FinalNotSupported))
  to S_Ι = (L_ι, (Uppercase, FinalNotSupported))
  to S_Κ = (L_κ, (Uppercase, FinalNotSupported))
  to S_Λ = (L_λ, (Uppercase, FinalNotSupported))
  to S_Μ = (L_μ, (Uppercase, FinalNotSupported))
  to S_Ν = (L_ν, (Uppercase, FinalNotSupported))
  to S_Ξ = (L_ξ, (Uppercase, FinalNotSupported))
  to S_Ο = (L_ο, (Uppercase, FinalNotSupported))
  to S_Π = (L_π, (Uppercase, FinalNotSupported))
  to S_Ρ = (L_ρ, (Uppercase, FinalNotSupported))
  to S_Σ = (L_σ, (Uppercase, FinalNotSupported))
  to S_Τ = (L_τ, (Uppercase, FinalNotSupported))
  to S_Υ = (L_υ, (Uppercase, FinalNotSupported))
  to S_Φ = (L_φ, (Uppercase, FinalNotSupported))
  to S_Χ = (L_χ, (Uppercase, FinalNotSupported))
  to S_Ψ = (L_ψ, (Uppercase, FinalNotSupported))
  to S_Ω = (L_ω, (Uppercase, FinalNotSupported))
  to S_α = (L_α, (Lowercase, FinalNotSupported))
  to S_β = (L_β, (Lowercase, FinalNotSupported))
  to S_γ = (L_γ, (Lowercase, FinalNotSupported))
  to S_δ = (L_δ, (Lowercase, FinalNotSupported))
  to S_ε = (L_ε, (Lowercase, FinalNotSupported))
  to S_ζ = (L_ζ, (Lowercase, FinalNotSupported))
  to S_η = (L_η, (Lowercase, FinalNotSupported))
  to S_θ = (L_θ, (Lowercase, FinalNotSupported))
  to S_ι = (L_ι, (Lowercase, FinalNotSupported))
  to S_κ = (L_κ, (Lowercase, FinalNotSupported))
  to S_λ = (L_λ, (Lowercase, FinalNotSupported))
  to S_μ = (L_μ, (Lowercase, FinalNotSupported))
  to S_ν = (L_ν, (Lowercase, FinalNotSupported))
  to S_ξ = (L_ξ, (Lowercase, FinalNotSupported))
  to S_ο = (L_ο, (Lowercase, FinalNotSupported))
  to S_π = (L_π, (Lowercase, FinalNotSupported))
  to S_ρ = (L_ρ, (Lowercase, FinalNotSupported))
  to S_σ = (L_σ, (Lowercase, IsNotFinal))
  to S_ς = (L_σ, (Lowercase, IsFinal))
  to S_τ = (L_τ, (Lowercase, FinalNotSupported))
  to S_υ = (L_υ, (Lowercase, FinalNotSupported))
  to S_φ = (L_φ, (Lowercase, FinalNotSupported))
  to S_χ = (L_χ, (Lowercase, FinalNotSupported))
  to S_ψ = (L_ψ, (Lowercase, FinalNotSupported))
  to S_ω = (L_ω, (Lowercase, FinalNotSupported))

  from (L_α, (Uppercase, FinalNotSupported)) = Success S_Α
  from (L_β, (Uppercase, FinalNotSupported)) = Success S_Β
  from (L_γ, (Uppercase, FinalNotSupported)) = Success S_Γ
  from (L_δ, (Uppercase, FinalNotSupported)) = Success S_Δ
  from (L_ε, (Uppercase, FinalNotSupported)) = Success S_Ε
  from (L_ζ, (Uppercase, FinalNotSupported)) = Success S_Ζ
  from (L_η, (Uppercase, FinalNotSupported)) = Success S_Η
  from (L_θ, (Uppercase, FinalNotSupported)) = Success S_Θ
  from (L_ι, (Uppercase, FinalNotSupported)) = Success S_Ι
  from (L_κ, (Uppercase, FinalNotSupported)) = Success S_Κ
  from (L_λ, (Uppercase, FinalNotSupported)) = Success S_Λ
  from (L_μ, (Uppercase, FinalNotSupported)) = Success S_Μ
  from (L_ν, (Uppercase, FinalNotSupported)) = Success S_Ν
  from (L_ξ, (Uppercase, FinalNotSupported)) = Success S_Ξ
  from (L_ο, (Uppercase, FinalNotSupported)) = Success S_Ο
  from (L_π, (Uppercase, FinalNotSupported)) = Success S_Π
  from (L_ρ, (Uppercase, FinalNotSupported)) = Success S_Ρ
  from (L_σ, (Uppercase, FinalNotSupported)) = Success S_Σ
  from (L_τ, (Uppercase, FinalNotSupported)) = Success S_Τ
  from (L_υ, (Uppercase, FinalNotSupported)) = Success S_Υ
  from (L_φ, (Uppercase, FinalNotSupported)) = Success S_Φ
  from (L_χ, (Uppercase, FinalNotSupported)) = Success S_Χ
  from (L_ψ, (Uppercase, FinalNotSupported)) = Success S_Ψ
  from (L_ω, (Uppercase, FinalNotSupported)) = Success S_Ω
  from (L_α, (Lowercase, FinalNotSupported)) = Success S_α
  from (L_β, (Lowercase, FinalNotSupported)) = Success S_β
  from (L_γ, (Lowercase, FinalNotSupported)) = Success S_γ
  from (L_δ, (Lowercase, FinalNotSupported)) = Success S_δ
  from (L_ε, (Lowercase, FinalNotSupported)) = Success S_ε
  from (L_ζ, (Lowercase, FinalNotSupported)) = Success S_ζ
  from (L_η, (Lowercase, FinalNotSupported)) = Success S_η
  from (L_θ, (Lowercase, FinalNotSupported)) = Success S_θ
  from (L_ι, (Lowercase, FinalNotSupported)) = Success S_ι
  from (L_κ, (Lowercase, FinalNotSupported)) = Success S_κ
  from (L_λ, (Lowercase, FinalNotSupported)) = Success S_λ
  from (L_μ, (Lowercase, FinalNotSupported)) = Success S_μ
  from (L_ν, (Lowercase, FinalNotSupported)) = Success S_ν
  from (L_ξ, (Lowercase, FinalNotSupported)) = Success S_ξ
  from (L_ο, (Lowercase, FinalNotSupported)) = Success S_ο
  from (L_π, (Lowercase, FinalNotSupported)) = Success S_π
  from (L_ρ, (Lowercase, FinalNotSupported)) = Success S_ρ
  from (L_σ, (Lowercase, IsNotFinal)) = Success S_σ
  from (L_σ, (Lowercase, IsFinal)) = Success S_ς
  from (L_τ, (Lowercase, FinalNotSupported)) = Success S_τ
  from (L_υ, (Lowercase, FinalNotSupported)) = Success S_υ
  from (L_φ, (Lowercase, FinalNotSupported)) = Success S_φ
  from (L_χ, (Lowercase, FinalNotSupported)) = Success S_χ
  from (L_ψ, (Lowercase, FinalNotSupported)) = Success S_ψ
  from (L_ω, (Lowercase, FinalNotSupported)) = Success S_ω
  from (l, (c, f)) = Failure $ InvalidCombo l c f
