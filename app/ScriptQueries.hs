{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ScriptQueries where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2, _Left, toListOf, view, _Just)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import QueryStage
import Grammar.CommonTypes
import qualified Grammar.Greek.Stage as Stage
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import qualified Primary

getElision = pure . view (_2 . _1 . _2)

getLetterMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* [Mark]]
getLetterMarks = view (_2 . _1 . _1 . _1)

getMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Mark]]
getMarks = over traverse snd . fst . fst . fst . snd

getLetterSyllabicMark
  :: ctx :* ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* Maybe SyllabicMark]
getLetterSyllabicMark = over traverse (\(l, (_, (_, sm))) -> (l, sm)) . fst . fst . fst . snd

getVowelMarks
  :: ctx
    :* ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark) :+ ConsonantRho ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
getVowelMarks = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getVowelMarkGroups
  :: ctx
    :* ((([ [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
      :+ [ConsonantRho]
      ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]]
getVowelMarkGroups = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getCrasis
  :: ctx :* a :* b :* c :* Crasis :* d
  -> [Crasis]
getCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

getMarkPreservation
  :: ctx :* a :* b :* MarkPreservation :* c
  -> [MarkPreservation]
getMarkPreservation = toListOf (_2 . _2 . _2 . _1)

toAccentReverseIndex :: [Maybe ContextualAccent] -> [(Int, ContextualAccent)]
toAccentReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe ContextualAccent)] -> [(Int, ContextualAccent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []

getAccentReverseIndexPunctuation
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision
  -> [[Int :* ContextualAccent] :* HasWordPunctuation]
getAccentReverseIndexPunctuation = pure . over _1 goAll . getPair
  where
  goAll = toAccentReverseIndex . getAccents

  getPair
    :: m :* ([ a :* Maybe ContextualAccent ] :* HasWordPunctuation) :* b
    -> [ a :* Maybe ContextualAccent ] :* HasWordPunctuation
  getPair x = (view (_2 . _1 . _1) x, view (_2 . _1 . _2) x)

  getAccents :: [ a :* Maybe ContextualAccent ] -> [Maybe ContextualAccent]
  getAccents = over traverse snd

getAccentReverseIndex
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* a) :* b
  -> [[Int :* ContextualAccent]]
getAccentReverseIndex = pure . toAccentReverseIndex . fmap snd . view (_2 . _1 . _1)

getForceAcute
  :: ctx :* Word
  -> [ForceAcute]
getForceAcute = toListOf (_2 . _wordAccent . _Just . _accentForce)

getAccentPosition
  :: ctx :* Word
  -> [Maybe (BasicAccent :* AccentPosition)]
getAccentPosition = pure . over _Just (\x -> (accentValue x, accentPosition x)) . view (_2 . _wordAccent)

getFinalConsonants
  :: ctx :* Word
  -> [[ConsonantRho]]
getFinalConsonants = pure . view (_2 . _wordFinalConsonants)

getElisionSyllables
  :: ctx :* Word
  -> [InitialAspiration :* [Syllable] :* [ConsonantRho]]
getElisionSyllables = result
  where
  result x = case el x of
    IsElided -> [(asp x, (syll x, fin x))]
    NotElided -> []
  asp = view (_2 . _wordInitialAspiration)
  syll = view (_2 . _wordSyllables)
  fin = view (_2 . _wordFinalConsonants)
  el = view (_2 . _wordElision)

getFinalSyllable
  :: ctx :* Word
  -> [[Syllable] :* [ConsonantRho]]
getFinalSyllable = result
  where
  result x = pure (syll x, fin x)
  syll = reverse . List.take 1 . reverse . view (_2 . _wordSyllables)
  fin = view (_2 . _wordFinalConsonants)

queries :: Map String (QueryOptions -> [Primary.Group] -> IO ())
queries = Map.fromList
  [ ("elision", queryStage Stage.toElision getElision)
  , ("letter-marks", queryStage Stage.toMarkGroups getLetterMarks)
  , ("marks", queryStage Stage.toMarkGroups getMarks)
  , ("letter-syllabic-mark", queryStage Stage.toMarkSplit getLetterSyllabicMark)
  , ("vowel-marks", queryStage Stage.toConsonantMarks getVowelMarks)
  , ("vowel-mark-groups", queryStage Stage.toGroupVowelConsonants getVowelMarkGroups)
  , ("crasis", queryStage Stage.toBreathing getCrasis)
  , ("mark-preservation", queryStage Stage.toBreathing getMarkPreservation)
  , ("accent-reverse-index", queryStage Stage.toBreathing getAccentReverseIndex)
  , ("accent-reverse-index-punctuation", queryStage Stage.toBreathing getAccentReverseIndexPunctuation)
  , ("force-acute", queryStage Stage.script getForceAcute)
  , ("accent-position", queryStage Stage.script getAccentPosition)
  , ("final-consonants", queryStage Stage.script getFinalConsonants)
  , ("elision-syllables", queryStage Stage.script getElisionSyllables)
  , ("final-syllable", queryStage Stage.script getFinalSyllable)
  ]
