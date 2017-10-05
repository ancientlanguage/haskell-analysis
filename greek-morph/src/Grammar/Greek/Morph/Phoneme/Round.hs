module Grammar.Greek.Morph.Phoneme.Round where

import Prelude hiding (Word)
import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types hiding (Case)
import Grammar.Greek.Script.Word
import qualified Grammar.Common.Round as Round

coreWordToPhonemes :: CoreWord -> [Phoneme]
coreWordToPhonemes (CoreWord asp ss fc) = addAsp asp $ concatMap doSyll ss ++ doCons fc
  where
  addAsp NoInitialAspiration xs = xs
  addAsp HasInitialAspiration xs = Ph_Aspiration : xs
  doSyll (Syllable cs v) = doCons cs ++ [Ph_Vocalic v]
  doCons = fmap Ph_Consonant

simplePhonemesToSyllables :: [ConsonantRho :+ VocalicSyllable] -> [[ConsonantRho] :* VocalicSyllable] :* [ConsonantRho]
simplePhonemesToSyllables = roundIdTo Round.groupLeft

simplePhoneme :: Phoneme -> [ConsonantRho :+ VocalicSyllable]
simplePhoneme Ph_Aspiration = []
simplePhoneme (Ph_Consonant x) = [Left x]
simplePhoneme (Ph_Vocalic x) = [Right x]

phonemesToCoreWord :: [Phoneme] -> CoreWord
phonemesToCoreWord [] = CoreWord NoInitialAspiration [] []
phonemesToCoreWord xs@(x : _) =
  let
    ps = concatMap simplePhoneme xs
    (sps, fc) = simplePhonemesToSyllables ps
    ss = fmap (uncurry Syllable) sps
    asp = case x of
      Ph_Aspiration -> HasInitialAspiration
      _ -> NoInitialAspiration
  in CoreWord asp ss fc

phoneme :: RoundId CoreWord [Phoneme]
phoneme = RoundId coreWordToPhonemes phonemesToCoreWord
