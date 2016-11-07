module Grammar.Contextualize where

import Grammar.CommonTypes

contextualize :: Int -> [a] -> [a :* [a] :* [a]]
contextualize n = contextualizeAccum n []

contextualizeAccum :: Int -> [a] -> [a] -> [a :* [a] :* [a]]
contextualizeAccum _ _ [] = []
contextualizeAccum n ps (x : xs) = (x, (take n ps, take n xs)) : contextualizeAccum n (take n (x : ps)) xs
