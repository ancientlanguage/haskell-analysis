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
import Grammar.Greek.Script.Around
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

forget
  :: [Milestone :* [a] :* SentenceBoundary]
  -> [Milestone :* [a]]
forget = over (traverse . _2) fst

type AroundMilestone e1 e2 a b =
  Around
  (Milestone :* a :* e1)
  (Milestone :* b :* e2)
  [Milestone :* a]
  [Milestone :* b]

around0 :: AroundMilestone InvalidChar Void
  ([Char] :* SentenceBoundary)
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
around0 = Around
  (milestoneContext . _1 . travList $ aroundTo unicodeSymbol)
  (milestoneContext . _1 . travList $ aroundFrom unicodeSymbol)

around10 :: AroundMilestone Void Void
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
around10 = Around
  (milestoneContext . _1 . travList $ aroundTo aroundSumAssoc12_3)
  (milestoneContext . _1 . travList $ aroundFrom aroundSumAssoc12_3)

around20 :: AroundMilestone InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
around20 = Around
  (milestoneContext . _1 $ aroundTo wordPunctuationElision)
  (milestoneContext . _1 $ aroundFrom wordPunctuationElision)

around30 :: AroundMilestone Void InvalidLetterCaseFinal
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
around30 = Around
  (milestoneContext . _1 . _1 . travList . _Left $ aroundTo symbolLetter)
  (milestoneContext . _1 . _1 . travList . _Left $ aroundFrom symbolLetter)

around40 :: AroundMilestone InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
around40 = Around
  (milestoneContext . _1 . _1 $ aroundTo markGroups)
  (milestoneContext . _1 . _1 $ aroundFrom markGroups)

around50 :: AroundMilestone InvalidFinals Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case) :* [Mark]] :* Elision) :* SentenceBoundary)
around50 = Around
  (milestoneContext . _1 . _1 $ aroundTo final)
  (milestoneContext . _1 . _1 $ aroundFrom final)

stage = Stage allAround forget
  where
    allAround
      = around0
      <+> around10
      <+> around20
      <+> around30
      <+> around40
      <+> around50
    (<+>) = joinAround'
    infixr 6 <+>
