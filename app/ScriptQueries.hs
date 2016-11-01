{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ScriptQueries where

import Control.Lens (over, _1, _2, _Left, toListOf, view, _Just)
import Data.Map (Map)
import qualified Data.Map as Map

import QueryStage
import Grammar.CommonTypes
import qualified Grammar.Greek.Stage as Stage
import Grammar.Greek.Script.Types
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
  :: ctx :* ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* Maybe SyllabicMark]
getLetterSyllabicMark = over traverse (\(l, (_, (_, sm))) -> (l, sm)) . fst . fst . fst . snd

getVowelMarks
  :: ctx
    :* ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark) :+ ConsonantRho ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
getVowelMarks = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getVowelMarkGroups
  :: ctx
    :* ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
      :+ [ConsonantRho]
      ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]]
getVowelMarkGroups = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getCrasis
  :: ctx :* a :* b :* c :* Crasis :* d
  -> [Crasis]
getCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

getMarkPreservation
  :: ctx :* a :* b :* MarkPreservation :* c
  -> [MarkPreservation]
getMarkPreservation = toListOf (_2 . _2 . _2 . _1)

toAccentReverseIndex :: [Maybe Accent] -> [(Int, Accent)]
toAccentReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe Accent)] -> [(Int, Accent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []

getAccentReverseIndexPunctuation
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe Accent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision
  -> [[Int :* Accent] :* HasWordPunctuation]
getAccentReverseIndexPunctuation = pure . over _1 goAll . getPair
  where
  goAll = toAccentReverseIndex . getAccents

  getPair
    :: m :* ([ a :* Maybe Accent ] :* HasWordPunctuation) :* b
    -> [ a :* Maybe Accent ] :* HasWordPunctuation
  getPair x = (view (_2 . _1 . _1) x, view (_2 . _1 . _2) x)

  getAccents :: [ a :* Maybe Accent ] -> [Maybe Accent]
  getAccents = over traverse snd

getAccentReverseIndex
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe Accent ] :* a) :* b
  -> [[Int :* Accent]]
getAccentReverseIndex = pure . toAccentReverseIndex . fmap snd . view (_2 . _1 . _1)

getForceAcute
  :: ctx :* (a :* Maybe (b :* c :* ForceAcute :* d) :* e) :* f
  -> [ForceAcute]
getForceAcute = toListOf (_2 . _1 . _2 . _1 . _Just . _2 . _2 . _1)

getAccentPosition
  :: ctx :* (a :* Maybe (WordAccent :* AccentPosition :* b :* c) :* d) :* e
  -> [Maybe (WordAccent :* AccentPosition)]
getAccentPosition = pure . over (_Just . _2) fst . view (_2 . _1 . _2 . _1)

getFinalConsonants
  :: ctx :* a :* [ConsonantRho] :* b
  -> [[ConsonantRho]]
getFinalConsonants = pure . view (_2 . _2 . _1)

getElisionSyllables
  :: ctx
    :* ([[ConsonantRho] :* VocalicSyllable] :* Maybe (WordAccent :* AccentPosition :* ForceAcute :* ExtraAccents) :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision
  -> [InitialAspiration :* [[ConsonantRho] :* VocalicSyllable] :* [ConsonantRho]]
getElisionSyllables = result
  where
  result x = case el x of
    IsElided -> [(asp x, (syll x, fin x))]
    NotElided -> []
  asp = view (_2 . _2 . _2 . _2 . _2 . _1)
  syll = view (_2 . _1 . _1)
  fin = view (_2 . _2 . _1)
  el = view (_2 . _2 . _2 . _2 . _2 . _2 . _2)

queries :: Map String (QueryOptions -> [Primary.Group] -> IO ())
queries = Map.fromList
  [ ("Elision", queryStage Stage.toElision getElision)
  , ("LetterMarks", queryStage Stage.toMarkGroups getLetterMarks)
  , ("Marks", queryStage Stage.toMarkGroups getMarks)
  , ("LetterSyllabicMark", queryStage Stage.toMarkSplit getLetterSyllabicMark)
  , ("VowelMarks", queryStage Stage.toConsonantMarks getVowelMarks)
  , ("VowelMarkGroups", queryStage Stage.toGroupVowelConsonants getVowelMarkGroups)
  , ("Crasis", queryStage Stage.toBreathing getCrasis)
  , ("MarkPreservation", queryStage Stage.toBreathing getMarkPreservation)
  , ("AccentReverseIndex", queryStage Stage.toBreathing getAccentReverseIndex)
  , ("AccentReverseIndexPunctuation", queryStage Stage.toBreathing getAccentReverseIndexPunctuation)
  , ("ForceAcute", queryStage Stage.script getForceAcute)
  , ("AccentPosition", queryStage Stage.script getAccentPosition)
  , ("FinalConsonants", queryStage Stage.script getFinalConsonants)
  , ("ElisionSyllables", queryStage Stage.script getElisionSyllables)
  ]
