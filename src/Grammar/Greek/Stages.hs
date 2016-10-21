{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Stages where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Prepare
import Grammar.Greek.Script.SymbolLetter
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.UnicodeSymbol
import Control.Lens hiding ((:>))

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

around0 :: Around
  (Milestone :* ([Char] :* SentenceBoundary) :* InvalidChar)
  (Milestone :* ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* Void)
  [Milestone :* [Char] :* SentenceBoundary]
  [Milestone :* [Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary]
around0 = Around
  (travList . withItemContext . _1 . travList $ aroundTo unicodeSymbol)
  (travList . withItemContext . _1 . travList $ aroundFrom unicodeSymbol)

around1 :: Around
  (Milestone :* ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* Void)
  (Milestone :* ([(Letter :* Case :* Final) :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* InvalidLetterCaseFinal)
  [Milestone :* [Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary]
  [Milestone :* [(Letter :* Case :* Final) :+ Mark :+ WordPunctuation] :* SentenceBoundary]
around1 = Around
  (milestoneContext . _1 . travList . _Left $ aroundTo symbolLetter)
  (milestoneContext . _1 . travList . _Left $ aroundFrom symbolLetter)

stage = Stage (joinAround' around0 around1) forget
