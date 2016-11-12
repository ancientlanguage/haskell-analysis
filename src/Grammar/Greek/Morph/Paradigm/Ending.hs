module Grammar.Greek.Morph.Paradigm.Ending where

import Control.Lens (_2)
import qualified Data.List as List
import Data.Maybe
import Grammar.Common
import Grammar.Greek.Morph.Phoneme.Round
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

allSplitsList :: [a] -> [[a] :* [a]]
allSplitsList = go []
  where
  go xs [] = [xs :^ []]
  go xs ys@(y : ys') = xs :^ ys : go (xs ++ [y]) ys'

longestPrefix :: Eq a => [[a]] -> [a]
longestPrefix [] = []
longestPrefix (x : xs) = go (reverse . fmap fst . allSplitsList $ x) xs
  where
  go [] _ = []
  go (p : ps) ys =
    if and (fmap (List.isPrefixOf p) ys)
    then p
    else go ps ys

nounParadigmForms :: Gender -> NounParadigm -> [Morph :* ParadigmExemplar]
nounParadigmForms g (NounParadigm sgNom sgGen sgDat sgAcc sgVoc duNAV duGD plNomV plGen plDat plAcc) = catMaybes . fmap (_2 id) $
  [ substantiveMorph Nominative Singular g :^ sgNom
  , substantiveMorph Genitive Singular g :^ sgGen
  , substantiveMorph Dative Singular g :^ sgDat
  , substantiveMorph Accusative Singular g :^ sgAcc
  , substantiveMorph Vocative Singular g :^ sgVoc
  ]
  ++
  fmap (\c -> substantiveMorph c Dual g :^ duNAV) [Nominative, Accusative, Vocative]
  ++
  fmap (\c -> substantiveMorph c Dual g :^ duGD) [Genitive, Dative]
  ++
  fmap (\c -> substantiveMorph c Plural g :^ plNomV) [Nominative, Vocative]
  ++
  [ substantiveMorph Genitive Plural g :^ plGen
  , substantiveMorph Dative Plural g :^ plDat
  , substantiveMorph Accusative Plural g :^ plAcc
  ]

{-
-- cda
-- [][] [cda][]
-- [][c] [da][]
-- [][cd] [a][]
-- [cda][] [][]
allSplitsSyllable
  :: Syllable
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
allSplitsSyllable s@(Syllable cs v)
  = fmap (\(ls, rs) -> ([] :^ ls) :^ ([Syllable rs v] :^ [])) (allSplitsList cs)
  ++ [([s] :^ []) :^ ([] :^ [])]

mergeSyllableWithFinalConsonants
  :: [Syllable]
  -> Syllable
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
mergeSyllableWithFinalConsonants _ _ [] = []
mergeSyllableWithFinalConsonants ss s ((([], fc), rs) : xs) = ([s] :^ fc) :^ rs ++ mergeSyllableWithFinalConsonants s xs
mergeSyllableWithFinalConsonants _ _ xs@(((_ : _, _), _) : _) = xs

concatUniteFinals
  :: [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
concatUniteFinals xs ys = case reverse xs :^ ys of
  (((s :^ []) :^ ([] :^ [])) : xs') :^ (_ : _) -> reverse xs' ++ mergeSyllableWithFinalConsonants s ys
  _ -> xs ++ ys

allSplitsSyllableList
  :: [Syllable]
  -> [([Syllable] :* [ConsonantRho]) :* ([Syllable] :* [ConsonantRho])]
allSplitsSyllableList [] = [([] :^ []) :^ ([] :^ [])]
allSplitsSyllableList (s : ss) = concatUniteFinals (allSplitsSyllable s) (allSplitsSyllableList ss)

splitsAddFinalConsonants :: [CoreWord :* CoreWord] -> [ConsonantRho] -> [CoreWord :* CoreWord]
splitsAddFinalConsonants = undefined

allSplits :: CoreWord -> [[CoreWord :* CoreWord]]
allSplits (CoreWord asp [] fc) = []
allSplits (CoreWord asp (s :: ss) fc) = []

extractEndings :: [Maybe ParadigmForm] -> Either String [Maybe ParadigmEnding]
extractEndings _ = Right []
-}
