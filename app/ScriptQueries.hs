{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ScriptQueries where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2, _Left, toListOf, view, _Just)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Grammar.IO.QueryStage
import Grammar.CommonTypes
import qualified Grammar.Greek.Script.Stage as Stage
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import qualified Primary

queryElision = pure . view (_2 . _1 . _2)

queryLetterMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* [Mark]]
queryLetterMarks = view (_2 . _1 . _1 . _1)

queryMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Mark]]
queryMarks = over traverse snd . fst . fst . fst . snd

queryLetterSyllabicMark
  :: ctx :* ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* Maybe SyllabicMark]
queryLetterSyllabicMark = over traverse (\(l, (_, (_, sm))) -> (l, sm)) . fst . fst . fst . snd

queryVowelMarks
  :: ctx
    :* ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark) :+ ConsonantRho ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
queryVowelMarks = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

queryVowelMarkGroups
  :: ctx
    :* ((([ [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
      :+ [ConsonantRho]
      ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]]
queryVowelMarkGroups = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

queryCrasis
  :: ctx :* a :* b :* c :* Crasis :* d
  -> [Crasis]
queryCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

queryMarkPreservation
  :: ctx :* a :* b :* MarkPreservation :* c
  -> [MarkPreservation]
queryMarkPreservation = toListOf (_2 . _2 . _2 . _1)

toAccentReverseIndex :: [Maybe ContextualAccent] -> [(Int, ContextualAccent)]
toAccentReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe ContextualAccent)] -> [(Int, ContextualAccent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []

queryAccentReverseIndexPunctuation
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision
  -> [[Int :* ContextualAccent] :* HasWordPunctuation]
queryAccentReverseIndexPunctuation = pure . over _1 goAll . getPair
  where
  goAll = toAccentReverseIndex . getAccents

  getPair
    :: m :* ([ a :* Maybe ContextualAccent ] :* HasWordPunctuation) :* b
    -> [ a :* Maybe ContextualAccent ] :* HasWordPunctuation
  getPair x = (view (_2 . _1 . _1) x, view (_2 . _1 . _2) x)

  getAccents :: [ a :* Maybe ContextualAccent ] -> [Maybe ContextualAccent]
  getAccents = over traverse snd

queryAccentReverseIndex
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* a) :* b
  -> [[Int :* ContextualAccent]]
queryAccentReverseIndex = pure . toAccentReverseIndex . fmap snd . view (_2 . _1 . _1)

queryForceAcute
  :: ctx :* Word
  -> [ForceAcute]
queryForceAcute = toListOf (_2 . _wordAccent . _Just . _accentForce)

queryAccentPosition
  :: ctx :* Word
  -> [Maybe (BasicAccent :* AccentPosition)]
queryAccentPosition = pure . over _Just (\x -> (accentValue x, accentPosition x)) . view (_2 . _wordAccent)

queryFinalConsonants
  :: ctx :* Word
  -> [[ConsonantRho]]
queryFinalConsonants = pure . view (_2 . _wordFinalConsonants)

queryElisionSyllables
  :: ctx :* Word
  -> [InitialAspiration :* [Syllable] :* [ConsonantRho]]
queryElisionSyllables = result
  where
  result x = case el x of
    IsElided -> [(asp x, (syll x, fin x))]
    NotElided -> []
  asp = view (_2 . _wordInitialAspiration)
  syll = view (_2 . _wordSyllables)
  fin = view (_2 . _wordFinalConsonants)
  el = view (_2 . _wordElision)

queryFinalSyllable
  :: ctx :* Word
  -> [[Syllable] :* [ConsonantRho]]
queryFinalSyllable = result
  where
  result x = pure (syll x, fin x)
  syll = reverse . List.take 1 . reverse . view (_2 . _wordSyllables)
  fin = view (_2 . _wordFinalConsonants)

queryIndependentSyllables :: ctx :* Word -> [Syllable]
queryIndependentSyllables = toListOf (_2 . _wordSyllables . traverse)

getInitialSyllable :: Word -> InitialAspiration :* [Syllable]
getInitialSyllable w = (wordInitialAspiration w, take 1 . wordSyllables $ w)

getFinalSyllable :: Word -> [Syllable] :* [ConsonantRho]
getFinalSyllable w = (take 1 . reverse . wordSyllables $ w, wordFinalConsonants w)

uncurrySyllable :: Syllable -> [ConsonantRho] :* VocalicSyllable
uncurrySyllable (Syllable c v) = (c, v)

getInitialVocalicSyllable :: Word -> [InitialAspiration :* VocalicSyllable]
getInitialVocalicSyllable w = result
  where
  result = case ss of
    (Syllable [] v : _) -> pure (asp, v)
    _ -> []
  (asp, ss) = getInitialSyllable w

queryElisionNextSyllable
  :: (ctx :* Word) :* [ctx :* Word] :* [ctx :* Word]
  -> [[[ConsonantRho] :* VocalicSyllable] :* [ConsonantRho] :* [InitialAspiration :* [VocalicSyllable]]]
queryElisionNextSyllable (w, (_, nws)) = ens
  where
  ens = case view (_2 . _wordElision) w of
    IsElided -> pure (fmap uncurrySyllable . snd . getInitialSyllable $ snd w, (fc, mn))
    NotElided -> []
  fc = view (_2 . _wordFinalConsonants) w
  mn = case nws of
    [] -> []
    (nw : _) -> pure (view (_2 . _wordInitialAspiration) nw, fmap (snd . uncurrySyllable) . take 1 . view (_2 . _wordSyllables) $ nw)

queryDeNext
  :: (ctx :* Word) :* [ctx :* Word] :* [ctx :* Word]
  -> [() :* [InitialAspiration :* VocalicSyllable]]
queryDeNext (w, (_, n)) =
  case wordSyllables (snd w) of
    [Syllable [CR_δ] (VS_Vowel V_ε)] -> pure ((), concatMap (getInitialVocalicSyllable . snd) . take 1 $ n)
    _ -> []

queries :: Map String (QueryOptions -> [Primary.Group] -> IO ())
queries = Map.fromList
  [ ("elision", queryStage Stage.toElision queryElision)
  , ("letter-marks", queryStage Stage.toMarkGroups queryLetterMarks)
  , ("marks", queryStage Stage.toMarkGroups queryMarks)
  , ("letter-syllabic-mark", queryStage Stage.toMarkSplit queryLetterSyllabicMark)
  , ("vowel-marks", queryStage Stage.toConsonantMarks queryVowelMarks)
  , ("vowel-mark-groups", queryStage Stage.toGroupVowelConsonants queryVowelMarkGroups)
  , ("crasis", queryStage Stage.toBreathing queryCrasis)
  , ("mark-preservation", queryStage Stage.toBreathing queryMarkPreservation)
  , ("accent-reverse-index", queryStage Stage.toBreathing queryAccentReverseIndex)
  , ("accent-reverse-index-punctuation", queryStage Stage.toBreathing queryAccentReverseIndexPunctuation)
  , ("force-acute", queryStage Stage.script queryForceAcute)
  , ("accent-position", queryStage Stage.script queryAccentPosition)
  , ("final-consonants", queryStage Stage.script queryFinalConsonants)
  , ("elision-syllables", queryStage Stage.script queryElisionSyllables)
  , ("final-syllable", queryStage Stage.script queryFinalSyllable)
  , ("independent-syllables", queryStage Stage.script queryIndependentSyllables)
  , ("elision-next-syllable", queryStageContext 1 Stage.script queryElisionNextSyllable)
  , ("de-next", queryStageContext 1 Stage.script queryDeNext)
  ]
