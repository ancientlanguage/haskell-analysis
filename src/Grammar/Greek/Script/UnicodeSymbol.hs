module Grammar.Greek.Script.UnicodeSymbol where

import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidChar = InvalidChar Char
  deriving (Show)

pattern MatchSymbol :: a -> a :+ b
pattern MatchSymbol x <- Left x

pattern MatchMark :: b -> a :+ b :+ c 
pattern MatchMark x <- Right (Left x)

pattern MatchPunct :: c -> a :+ b :+ c
pattern MatchPunct x <- Right (Right x)

unicodeSymbol :: ParseAround InvalidChar Char (Symbol :+ Mark :+ WordPunctuation)
unicodeSymbol = makeParseAround to from
  where
  validSymbol = Right . Left
  validMark = Right . Right . Left
  validPunct = Right . Right . Right
  invalidChar = Left . InvalidChar
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
  to '\x0300' = validMark M_Grave -- COMBINING GRAVE ACCENT
  to '\x0301' = validMark M_Acute -- COMBINING ACUTE ACCENT
  to '\x0308' = validMark M_Diaeresis -- COMBINING DIAERESIS
  to '\x0313' = validMark M_Smooth -- COMBINING COMMA ABOVE
  to '\x0314' = validMark M_Rough -- COMBINING REVERSED COMMA ABOVE
  to '\x0342' = validMark M_Circumflex -- COMBINING GREEK PERISPOMENI
  to '\x0345' = validMark M_IotaSubscript -- COMBINING GREEK YPOGEGRAMMENI
  to '\x2019' = validPunct P_RightQuote
  to x = invalidChar x

  from (MatchSymbol S_Α) = 'Α'
  from (MatchSymbol S_Β) = 'Β'
  from (MatchSymbol S_Γ) = 'Γ'
  from (MatchSymbol S_Δ) = 'Δ'
  from (MatchSymbol S_Ε) = 'Ε'
  from (MatchSymbol S_Ζ) = 'Ζ'
  from (MatchSymbol S_Η) = 'Η'
  from (MatchSymbol S_Θ) = 'Θ'
  from (MatchSymbol S_Ι) = 'Ι'
  from (MatchSymbol S_Κ) = 'Κ'
  from (MatchSymbol S_Λ) = 'Λ'
  from (MatchSymbol S_Μ) = 'Μ'
  from (MatchSymbol S_Ν) = 'Ν'
  from (MatchSymbol S_Ξ) = 'Ξ'
  from (MatchSymbol S_Ο) = 'Ο'
  from (MatchSymbol S_Π) = 'Π'
  from (MatchSymbol S_Ρ) = 'Ρ'
  from (MatchSymbol S_Σ) = 'Σ'
  from (MatchSymbol S_Τ) = 'Τ'
  from (MatchSymbol S_Υ) = 'Υ'
  from (MatchSymbol S_Φ) = 'Φ'
  from (MatchSymbol S_Χ) = 'Χ'
  from (MatchSymbol S_Ψ) = 'Ψ'
  from (MatchSymbol S_Ω) = 'Ω'
  from (MatchSymbol S_α) = 'α'
  from (MatchSymbol S_β) = 'β'
  from (MatchSymbol S_γ) = 'γ'
  from (MatchSymbol S_δ) = 'δ'
  from (MatchSymbol S_ε) = 'ε'
  from (MatchSymbol S_ζ) = 'ζ'
  from (MatchSymbol S_η) = 'η'
  from (MatchSymbol S_θ) = 'θ'
  from (MatchSymbol S_ι) = 'ι'
  from (MatchSymbol S_κ) = 'κ'
  from (MatchSymbol S_λ) = 'λ'
  from (MatchSymbol S_μ) = 'μ'
  from (MatchSymbol S_ν) = 'ν'
  from (MatchSymbol S_ξ) = 'ξ'
  from (MatchSymbol S_ο) = 'ο'
  from (MatchSymbol S_π) = 'π'
  from (MatchSymbol S_ρ) = 'ρ'
  from (MatchSymbol S_σ) = 'σ'
  from (MatchSymbol S_ς) = 'ς'
  from (MatchSymbol S_τ) = 'τ'
  from (MatchSymbol S_υ) = 'υ'
  from (MatchSymbol S_φ) = 'φ'
  from (MatchSymbol S_χ) = 'χ'
  from (MatchSymbol S_ψ) = 'ψ'
  from (MatchSymbol S_ω) = 'ω'
  from (MatchMark M_Grave) = '\x0300' -- COMBINING GRAVE ACCENT
  from (MatchMark M_Acute) = '\x0301' -- COMBINING ACUTE ACCENT
  from (MatchMark M_Diaeresis) = '\x0308' -- COMBINING DIAERESIS
  from (MatchMark M_Smooth) = '\x0313' -- COMBINING COMMA ABOVE
  from (MatchMark M_Rough) = '\x0314' -- COMBINING REVERSED COMMA ABOVE
  from (MatchMark M_Circumflex) = '\x0342' -- COMBINING GREEK PERISPOMENI
  from (MatchMark M_IotaSubscript) = '\x0345' -- COMBINING GREEK YPOGEGRAMMENI
  from (MatchPunct P_RightQuote) = '\x2019'
