module Grammar.Greek.Morph.Aspirate where

import Grammar.Greek.Script.Types

aspirate :: ConsonantRho -> ConsonantRho
aspirate CR_κ = CR_χ
aspirate CR_π = CR_φ
aspirate CR_τ = CR_θ
aspirate x = x

aspirateList :: [ConsonantRho] -> [ConsonantRho]
aspirateList xs | (c : cs) <- reverse xs = reverse $ aspirate c : cs
aspirateList x = x

unaspirate :: ConsonantRho -> ConsonantRho
unaspirate CR_χ = CR_κ
unaspirate CR_φ = CR_π
unaspirate CR_θ = CR_τ
unaspirate x = x
