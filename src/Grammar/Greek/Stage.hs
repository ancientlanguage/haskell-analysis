{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Stage where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Prepare
import qualified Grammar.Greek.Script.Around as Around
import Grammar.Greek.Script.Types
import Control.Lens

suffixSentence :: Text -> SentenceBoundary
suffixSentence x = go NotSentenceEnd $ Text.unpack x
  where
  go :: SentenceBoundary -> String -> SentenceBoundary
  go s [] = s
  go _ ('.' : _) = SentenceEnd
  go _ (';' : _) = SentenceEnd
  go s (_ : xs) = go s xs

wordWithSentence :: Primary.Word -> String :* SentenceBoundary
wordWithSentence (Primary.Word _ t s) = (Text.unpack t , suffixSentence s)

start
  :: [Primary.Group]
  -> [SourceId :* [Milestone :* String :* SentenceBoundary]]
start = over (traverse . _2 . traverse . _2) wordWithSentence . prepareGroups

travList :: Applicative f => (a -> f b) -> [a] -> f [b]
travList = traverse

forgetSentenceBoundary
  :: [Milestone :* [a] :* SentenceBoundary]
  -> [Milestone :* [a]]
forgetSentenceBoundary = over (traverse . _2) fst

type AroundMilestone e1 e2 a b =
  Around
  (Milestone :* a :* e1)
  (Milestone :* b :* e2)
  [Milestone :* a]
  [Milestone :* b]

unicodeSymbol :: AroundMilestone Around.InvalidChar Void
  ([Char] :* SentenceBoundary)
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
unicodeSymbol = Around
  (milestoneContext . _1 . travList $ aroundTo Around.unicodeSymbol)
  (milestoneContext . _1 . travList $ aroundFrom Around.unicodeSymbol)

assocSymbolMark_WordPunctuation :: AroundMilestone Void Void
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
assocSymbolMark_WordPunctuation = Around
  (milestoneContext . _1 . travList $ aroundTo aroundSumAssoc12_3)
  (milestoneContext . _1 . travList $ aroundFrom aroundSumAssoc12_3)

wordPunctuationElision :: AroundMilestone Around.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
wordPunctuationElision = Around
  (milestoneContext . _1 $ aroundTo Around.wordPunctuationElision)
  (milestoneContext . _1 $ aroundFrom Around.wordPunctuationElision)

symbolLetter :: AroundMilestone Void Void
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
symbolLetter = Around
  (milestoneContext . _1 . _1 . travList . _Left $ aroundTo Around.symbolLetter)
  (milestoneContext . _1 . _1 . travList . _Left $ aroundFrom Around.symbolLetter)

markGroups :: AroundMilestone Around.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
markGroups = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.markGroups)
  (milestoneContext . _1 . _1 $ aroundFrom Around.markGroups)

final :: AroundMilestone Around.InvalidFinals Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case) :* [Mark]] :* Elision) :* SentenceBoundary)
final = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.final)
  (milestoneContext . _1 . _1 $ aroundFrom Around.final)

capitalization :: AroundMilestone Around.InvalidUppercase Void
  (([(Letter :* Case) :* [Mark]] :* Elision) :* SentenceBoundary)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
capitalization = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.capitalization)
  (milestoneContext . _1 . _1 $ aroundFrom Around.capitalization)

markSplit :: AroundMilestone Around.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
markSplit = Around
  (milestoneContext . _1 . _1 . _1 . travList . _2 $ aroundTo Around.markSplit)
  (milestoneContext . _1 . _1 . _1 . travList . _2 $ aroundFrom Around.markSplit)

letterVowelConsonant :: AroundMilestone Void Void
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
letterVowelConsonant = Around
  (milestoneContext . _1 . _1 . _1 . travList . _1 $ aroundTo Around.letterVowelConsonant)
  (milestoneContext . _1 . _1 . _1 . travList . _1 $ aroundFrom Around.letterVowelConsonant)

toElision
  = unicodeSymbol
  <+> assocSymbolMark_WordPunctuation
  <+> wordPunctuationElision

toMarkGroups
  = toElision
  <+> symbolLetter
  <+> markGroups
  <+> final
  <+> capitalization

toMarkSplit
  = toMarkGroups
  <+> markSplit

script
  = toMarkSplit
  <+> letterVowelConsonant
