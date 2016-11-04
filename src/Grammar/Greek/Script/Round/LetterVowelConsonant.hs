module Grammar.Greek.Script.Round.LetterVowelConsonant where

import Grammar.Round
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

letterVowelConsonant :: RoundId Letter (Vowel :+ Consonant)
letterVowelConsonant = RoundId to from
  where
  to L_α = Left V_α
  to L_β = Right C_β
  to L_γ = Right C_γ
  to L_δ = Right C_δ
  to L_ε = Left V_ε
  to L_ζ = Right C_ζ
  to L_η = Left V_η
  to L_θ = Right C_θ
  to L_ι = Left V_ι
  to L_κ = Right C_κ
  to L_λ = Right C_λ
  to L_μ = Right C_μ
  to L_ν = Right C_ν
  to L_ξ = Right C_ξ
  to L_ο = Left V_ο
  to L_π = Right C_π
  to L_ρ = Right C_ρ
  to L_σ = Right C_σ
  to L_τ = Right C_τ
  to L_υ = Left V_υ
  to L_φ = Right C_φ
  to L_χ = Right C_χ
  to L_ψ = Right C_ψ
  to L_ω = Left V_ω

  from (Left V_α) = L_α
  from (Right C_β) = L_β
  from (Right C_γ) = L_γ
  from (Right C_δ) = L_δ
  from (Left V_ε) = L_ε
  from (Right C_ζ) = L_ζ
  from (Left V_η) = L_η
  from (Right C_θ) = L_θ
  from (Left V_ι) = L_ι
  from (Right C_κ) = L_κ
  from (Right C_λ) = L_λ
  from (Right C_μ) = L_μ
  from (Right C_ν) = L_ν
  from (Right C_ξ) = L_ξ
  from (Left V_ο) = L_ο
  from (Right C_π) = L_π
  from (Right C_ρ) = L_ρ
  from (Right C_σ) = L_σ
  from (Right C_τ) = L_τ
  from (Left V_υ) = L_υ
  from (Right C_φ) = L_φ
  from (Right C_χ) = L_χ
  from (Right C_ψ) = L_ψ
  from (Left V_ω) = L_ω
