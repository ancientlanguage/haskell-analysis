module Grammar.Greek.Stages where

import Prelude hiding (Word)
import Data.Either.Validation
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Prepare
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
  -> AllWords (String :* SentenceBoundary)
start = allWordsPathId wordWithSentence . prepareGroups

stage0To
  :: Around InvalidChar e2 Char (Symbol :+ Mark :+ WordPunctuation)
  -> AllWords (String :* SentenceBoundary)
  -> Validation
    [SourceId :* Milestone :* (String :* SentenceBoundary) :* InvalidChar]
    (AllWords ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary))
stage0To x = allWordsPath . _1 . traverse $ aroundTo x

stage0From
  :: Around e1 Void Char (Symbol :+ Mark :+ WordPunctuation)
  -> AllWords ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
  -> Validation
    [SourceId :* Milestone :* ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* Void]
    (AllWords (String :* SentenceBoundary))
stage0From x = allWordsPath . _1 . traverse $ aroundFrom x

stage0Around
  :: Around
    (SourceId :* Milestone :* (String :* SentenceBoundary) :* InvalidChar)
    (SourceId :* Milestone :* ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* Void)
    (AllWords (String :* SentenceBoundary))
    (AllWords ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary))
stage0Around = Around (stage0To unicodeSymbol) (stage0From unicodeSymbol)

stage0Forget :: AllWords (String :* SentenceBoundary) -> AllWords String
stage0Forget = allWordsPathId fst

stage0 :: Stage
  (SourceId :* Milestone :* (String :* SentenceBoundary) :* InvalidChar)
  (SourceId :* Milestone :* ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary) :* Void)
  (AllWords (String :* SentenceBoundary))
  (AllWords ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary))
  (AllWords String)
stage0 = Stage stage0Around stage0Forget
