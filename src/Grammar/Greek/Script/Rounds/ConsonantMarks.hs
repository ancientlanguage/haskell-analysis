module Grammar.Greek.Script.Rounds.ConsonantMarks where

import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidConsonantMarks = InvalidConsonantMarks (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
  deriving (Show)

consonantMarks :: RoundFwd InvalidConsonantMarks (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark) ConsonantRho
consonantMarks = makeRoundFwd to from
  where
  to (C_β, (Nothing, (Nothing, Nothing))) = Success CR_β
  to (C_γ, (Nothing, (Nothing, Nothing))) = Success CR_γ
  to (C_δ, (Nothing, (Nothing, Nothing))) = Success CR_δ
  to (C_ζ, (Nothing, (Nothing, Nothing))) = Success CR_ζ
  to (C_θ, (Nothing, (Nothing, Nothing))) = Success CR_θ
  to (C_κ, (Nothing, (Nothing, Nothing))) = Success CR_κ
  to (C_λ, (Nothing, (Nothing, Nothing))) = Success CR_λ
  to (C_μ, (Nothing, (Nothing, Nothing))) = Success CR_μ
  to (C_ν, (Nothing, (Nothing, Nothing))) = Success CR_ν
  to (C_ξ, (Nothing, (Nothing, Nothing))) = Success CR_ξ
  to (C_π, (Nothing, (Nothing, Nothing))) = Success CR_π
  to (C_ρ, (Nothing, (Nothing, Nothing))) = Success $ CR_ρ B_Smooth
  to (C_ρ, (Nothing, (Just b, Nothing))) = Success $ CR_ρ b
  to (C_σ, (Nothing, (Nothing, Nothing))) = Success CR_σ
  to (C_τ, (Nothing, (Nothing, Nothing))) = Success CR_τ
  to (C_φ, (Nothing, (Nothing, Nothing))) = Success CR_φ
  to (C_χ, (Nothing, (Nothing, Nothing))) = Success CR_χ
  to (C_ψ, (Nothing, (Nothing, Nothing))) = Success CR_ψ
  to (C_ϝ, (Nothing, (Nothing, Nothing))) = Success CR_ϝ
  to x = Failure $ InvalidConsonantMarks x

  from CR_β = (C_β, (Nothing, (Nothing, Nothing)))
  from CR_γ = (C_γ, (Nothing, (Nothing, Nothing)))
  from CR_δ = (C_δ, (Nothing, (Nothing, Nothing)))
  from CR_ζ = (C_ζ, (Nothing, (Nothing, Nothing)))
  from CR_θ = (C_θ, (Nothing, (Nothing, Nothing)))
  from CR_κ = (C_κ, (Nothing, (Nothing, Nothing)))
  from CR_λ = (C_λ, (Nothing, (Nothing, Nothing)))
  from CR_μ = (C_μ, (Nothing, (Nothing, Nothing)))
  from CR_ν = (C_ν, (Nothing, (Nothing, Nothing)))
  from CR_ξ = (C_ξ, (Nothing, (Nothing, Nothing)))
  from CR_π = (C_π, (Nothing, (Nothing, Nothing)))
  from (CR_ρ B_Smooth) = (C_ρ, (Nothing, (Nothing, Nothing)))
  from (CR_ρ B_Rough) = (C_ρ, (Nothing, (Just B_Rough, Nothing)))
  from CR_σ = (C_σ, (Nothing, (Nothing, Nothing)))
  from CR_τ = (C_τ, (Nothing, (Nothing, Nothing)))
  from CR_φ = (C_φ, (Nothing, (Nothing, Nothing)))
  from CR_χ = (C_χ, (Nothing, (Nothing, Nothing)))
  from CR_ψ = (C_ψ, (Nothing, (Nothing, Nothing)))
  from CR_ϝ = (C_ϝ, (Nothing, (Nothing, Nothing)))
