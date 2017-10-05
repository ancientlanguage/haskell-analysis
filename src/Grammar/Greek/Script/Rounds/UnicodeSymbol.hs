module Grammar.Greek.Script.Rounds.UnicodeSymbol where

import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidChar = InvalidChar Char
  deriving (Show)

symbolToChar :: Symbol -> Char
symbolToChar S_Α = 'Α'
symbolToChar S_Β = 'Β'
symbolToChar S_Γ = 'Γ'
symbolToChar S_Δ = 'Δ'
symbolToChar S_Ε = 'Ε'
symbolToChar S_Ζ = 'Ζ'
symbolToChar S_Η = 'Η'
symbolToChar S_Θ = 'Θ'
symbolToChar S_Ι = 'Ι'
symbolToChar S_Κ = 'Κ'
symbolToChar S_Λ = 'Λ'
symbolToChar S_Μ = 'Μ'
symbolToChar S_Ν = 'Ν'
symbolToChar S_Ξ = 'Ξ'
symbolToChar S_Ο = 'Ο'
symbolToChar S_Π = 'Π'
symbolToChar S_Ρ = 'Ρ'
symbolToChar S_Σ = 'Σ'
symbolToChar S_Τ = 'Τ'
symbolToChar S_Υ = 'Υ'
symbolToChar S_Φ = 'Φ'
symbolToChar S_Χ = 'Χ'
symbolToChar S_Ψ = 'Ψ'
symbolToChar S_Ω = 'Ω'
symbolToChar S_α = 'α'
symbolToChar S_β = 'β'
symbolToChar S_γ = 'γ'
symbolToChar S_δ = 'δ'
symbolToChar S_ε = 'ε'
symbolToChar S_ζ = 'ζ'
symbolToChar S_η = 'η'
symbolToChar S_θ = 'θ'
symbolToChar S_ι = 'ι'
symbolToChar S_κ = 'κ'
symbolToChar S_λ = 'λ'
symbolToChar S_μ = 'μ'
symbolToChar S_ν = 'ν'
symbolToChar S_ξ = 'ξ'
symbolToChar S_ο = 'ο'
symbolToChar S_π = 'π'
symbolToChar S_ρ = 'ρ'
symbolToChar S_σ = 'σ'
symbolToChar S_ς = 'ς'
symbolToChar S_τ = 'τ'
symbolToChar S_υ = 'υ'
symbolToChar S_φ = 'φ'
symbolToChar S_χ = 'χ'
symbolToChar S_ψ = 'ψ'
symbolToChar S_ω = 'ω'
symbolToChar S_ϝ = 'ϝ'


unicodeSymbol :: RoundFwd InvalidChar Char (Symbol :+ Mark :+ WordPunctuation)
unicodeSymbol = makeRoundFwd to from
  where
  validSymbol = Success . Left
  validMark = Success . Right . Left
  validPunct = Success . Right . Right
  invalidChar = Failure . InvalidChar
  to 'Α' = validSymbol S_Α
  to 'Β' = validSymbol S_Β
  to 'Γ' = validSymbol S_Γ
  to 'Δ' = validSymbol S_Δ
  to 'Ε' = validSymbol S_Ε
  to 'Ζ' = validSymbol S_Ζ
  to 'Η' = validSymbol S_Η
  to 'Θ' = validSymbol S_Θ
  to 'Ι' = validSymbol S_Ι
  to 'Κ' = validSymbol S_Κ
  to 'Λ' = validSymbol S_Λ
  to 'Μ' = validSymbol S_Μ
  to 'Ν' = validSymbol S_Ν
  to 'Ξ' = validSymbol S_Ξ
  to 'Ο' = validSymbol S_Ο
  to 'Π' = validSymbol S_Π
  to 'Ρ' = validSymbol S_Ρ
  to 'Σ' = validSymbol S_Σ
  to 'Τ' = validSymbol S_Τ
  to 'Υ' = validSymbol S_Υ
  to 'Φ' = validSymbol S_Φ
  to 'Χ' = validSymbol S_Χ
  to 'Ψ' = validSymbol S_Ψ
  to 'Ω' = validSymbol S_Ω
  to 'α' = validSymbol S_α
  to 'β' = validSymbol S_β
  to 'γ' = validSymbol S_γ
  to 'δ' = validSymbol S_δ
  to 'ε' = validSymbol S_ε
  to 'ζ' = validSymbol S_ζ
  to 'η' = validSymbol S_η
  to 'θ' = validSymbol S_θ
  to 'ι' = validSymbol S_ι
  to 'κ' = validSymbol S_κ
  to 'λ' = validSymbol S_λ
  to 'μ' = validSymbol S_μ
  to 'ν' = validSymbol S_ν
  to 'ξ' = validSymbol S_ξ
  to 'ο' = validSymbol S_ο
  to 'π' = validSymbol S_π
  to 'ρ' = validSymbol S_ρ
  to 'σ' = validSymbol S_σ
  to 'ς' = validSymbol S_ς
  to 'τ' = validSymbol S_τ
  to 'υ' = validSymbol S_υ
  to 'φ' = validSymbol S_φ
  to 'χ' = validSymbol S_χ
  to 'ψ' = validSymbol S_ψ
  to 'ω' = validSymbol S_ω
  to 'ϝ' = validSymbol S_ϝ
  to '\x0300' = validMark M_Grave -- COMBINING GRAVE ACCENT
  to '\x0301' = validMark M_Acute -- COMBINING ACUTE ACCENT
  to '\x0308' = validMark M_Diaeresis -- COMBINING DIAERESIS
  to '\x0313' = validMark M_Smooth -- COMBINING COMMA ABOVE
  to '\x0314' = validMark M_Rough -- COMBINING REVERSED COMMA ABOVE
  to '\x0342' = validMark M_Circumflex -- COMBINING GREEK PERISPOMENI
  to '\x0345' = validMark M_IotaSubscript -- COMBINING GREEK YPOGEGRAMMENI
  to '\x2019' = validPunct P_RightQuote
  to x = invalidChar x

  from (Left s) = symbolToChar s
  from (Right (Left M_Grave)) = '\x0300' -- COMBINING GRAVE ACCENT
  from (Right (Left M_Acute)) = '\x0301' -- COMBINING ACUTE ACCENT
  from (Right (Left M_Diaeresis)) = '\x0308' -- COMBINING DIAERESIS
  from (Right (Left M_Smooth)) = '\x0313' -- COMBINING COMMA ABOVE
  from (Right (Left M_Rough)) = '\x0314' -- COMBINING REVERSED COMMA ABOVE
  from (Right (Left M_Circumflex)) = '\x0342' -- COMBINING GREEK PERISPOMENI
  from (Right (Left M_IotaSubscript)) = '\x0345' -- COMBINING GREEK YPOGEGRAMMENI
  from (Right (Right P_RightQuote)) = '\x2019'
