module Grammar.Common.List where

import Grammar.Common.Types
import qualified Data.Map.Strict as Map

addIndex :: [a] -> [Int :* a]
addIndex = zip [0..]

addReverseIndex :: [a] -> [Int :* a]
addReverseIndex = snd . foldr go (0, [])
  where
  go x (i, xs) = (i + 1, (i, x) : xs)

groupPairs :: Ord k => [k :* v] -> [k :* [v]]
groupPairs = Map.assocs . foldr go Map.empty
  where
  go (k, v) m = case Map.lookup k m of
    Just vs -> Map.insert k (v : vs) m
    Nothing -> Map.insert k [v] m

groupConsecutivePairs :: Eq k => [k :* v] -> [k :* [v]]
groupConsecutivePairs = foldr go []
  where
  go (k, v) [] = [(k, [v])]
  go (k, v) ((k', vs) : xs) | k == k' = (k', v : vs) : xs
  go (k, v) xs = (k, [v]) : xs

contextualize :: Int -> [a] -> [a :* [a] :* [a]]
contextualize n = contextualizeAccum n []

contextualizeAccum :: Int -> [a] -> [a] -> [a :* [a] :* [a]]
contextualizeAccum _ _ [] = []
contextualizeAccum n ps (x : xs) = (x, (take n ps, take n xs)) : contextualizeAccum n (take n (x : ps)) xs
