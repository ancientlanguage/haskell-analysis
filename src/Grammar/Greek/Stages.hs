{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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

travList :: Applicative f => (a -> f b) -> [a] -> f [b]
travList = traverse

stage0 = Stage
  ( Around
    (allWordsPath . _1 . travList $ aroundTo unicodeSymbol)
    (allWordsPath . _1 . travList $ aroundFrom unicodeSymbol)
  )
  (allWordsPathId fst)
