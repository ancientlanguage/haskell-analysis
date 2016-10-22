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

around1 :: AroundMilestone Void InvalidLetterCaseFinal
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
  ([(Letter :* Case :* Final) :+ Mark :+ WordPunctuation] :* SentenceBoundary)
around1 = Around
  (milestoneContext . _1 . travList . _Left $ aroundTo symbolLetter)
  (milestoneContext . _1 . travList . _Left $ aroundFrom symbolLetter)

stage = Stage allAround forget
  where
    allAround
      = around0
      <+> around1
    (<+>) = joinAround'
    infixr 6 <+>
