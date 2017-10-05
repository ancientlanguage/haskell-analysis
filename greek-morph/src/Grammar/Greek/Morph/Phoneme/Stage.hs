module Grammar.Greek.Morph.Phoneme.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2)
import Data.Either.Validation
import Data.Void
import Grammar.Common
import qualified Grammar.Greek.Morph.Phoneme.Round as Round
import Grammar.Greek.Morph.Clitic.Types
import Grammar.Greek.Morph.Phoneme.Types
import Grammar.Greek.Morph.Types

phoneme :: forall ctx . RoundContext ctx Void Void
  WordClitic
  WordPhoneme
phoneme = Round to from
  where
  toExtract :: WordClitic -> WordClitic :* CoreWord
  toExtract w@(WordClitic cw _ _ _ _ _ _ _ _ _) = w :^ cw

  toApply :: WordClitic :* [Phoneme] -> WordPhoneme
  toApply (WordClitic _ a b c d e f g h i, ps) = WordPhoneme ps a b c d e f g h i

  to
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) toApply
    . (traverseWithItemContext . _2 $ liftRoundIdTo Round.phoneme)
    . over (traverse . _2) toExtract

  fromExtract :: WordPhoneme -> WordPhoneme :* [Phoneme]
  fromExtract w@(WordPhoneme ps _ _ _ _ _ _ _ _ _) = w :^ ps

  fromApply :: WordPhoneme :* CoreWord -> WordClitic
  fromApply (WordPhoneme _ a b c d e f g h i, cw) = WordClitic cw a b c d e f g h i

  from
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) fromApply
    . (traverseWithItemContext . _2 $ liftRoundIdFrom Round.phoneme)
    . over (traverse . _2) fromExtract
