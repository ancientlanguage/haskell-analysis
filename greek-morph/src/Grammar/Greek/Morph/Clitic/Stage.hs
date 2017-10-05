module Grammar.Greek.Morph.Clitic.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2)
import Data.Either.Validation
import Data.Void
import Grammar.Common
import qualified Grammar.Greek.Morph.Clitic.Round as Round
import Grammar.Greek.Morph.Clitic.Types
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Word

clitic :: forall ctx . RoundContext ctx Void Void
  Word
  WordClitic
clitic = Round to from
  where
  toExtract :: Word -> Word :* CoreWord
  toExtract w = w :^ wordToCoreWord w

  toApply :: Word :* CoreWord :* Enclitic :* Proclitic -> WordClitic
  toApply (Word _ _ _ a b c d e f g, (cw, (en, pro))) = WordClitic cw a en pro b c d e f g

  to
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) toApply
    . (traverseWithItemContext . _2 $ liftRoundIdTo Round.clitic)
    . over (traverse . _2) toExtract

  fromExtract :: WordClitic -> WordClitic :* CoreWord :* Enclitic :* Proclitic
  fromExtract w@(WordClitic cw _ en pro _ _ _ _ _ _) = w :^ cw :^ en :^ pro

  fromApply :: WordClitic :* CoreWord -> Word
  fromApply (WordClitic _ a _ _ b c d e f g, CoreWord asp ss fc) = Word asp ss fc a b c d e f g

  from
    = over (_Failure . traverse . _2 . _1) fst
    . over (_Success . traverse . _2) fromApply
    . (traverseWithItemContext . _2 $ liftRoundIdFrom Round.clitic)
    . over (traverse . _2) fromExtract
