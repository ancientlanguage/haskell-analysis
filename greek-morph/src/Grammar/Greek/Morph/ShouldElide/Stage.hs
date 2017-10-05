module Grammar.Greek.Morph.ShouldElide.Stage where

import Prelude hiding (Word)
import Control.Lens (over, _1, _2)
import Data.Either.Validation
import Grammar.Common
import qualified Grammar.Greek.Morph.ShouldElide.Round as Round
import Grammar.Greek.Morph.ShouldElide.Types
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

addAspirationContext :: forall ctx w. (w -> InitialAspiration) -> [ctx :* w] -> [ctx :* w :* InitialAspiration]
addAspirationContext f = fmap go . contextualize 1
  where
  goNext :: [ctx :* w] -> InitialAspiration
  goNext [] = NoInitialAspiration
  goNext ((_, w) : _) = f w

  go :: (ctx :* w) :* [ctx :* w] :* [ctx :* w]
    -> ctx :* w :* InitialAspiration
  go ((sid, w), (_, ns)) = sid :^ w :^ goNext ns

shouldElide :: forall ctx . RoundContext ctx Round.InvalidElisionForm Round.InvalidElisionCandidate
  Word
  WordShouldElide
shouldElide = Round to from
  where
  to = to4 . to3 . to2 . to1

  to1 :: [ctx :* Word] -> [ctx :* Word :* CoreWord :* Elision :* InitialAspiration]
  to1 = over (traverse . _2) (\(w, ia) -> w :^ wordToCoreWord w :^ wordElision w :^ ia) . addAspirationContext wordInitialAspiration

  to2 :: [ctx :* Word :* CoreWord :* Elision :* InitialAspiration]
    -> Validation
      [ctx :* ((Word :* (CoreWord :* (Elision :* InitialAspiration))) :* Round.InvalidElisionForm)]
      [ctx :* Word :* CoreWord :* ShouldElide :* InitialAspiration]
  to2 = traverseWithItemContext . _2 $ roundTo Round.shouldElide

  to3 = over (_Failure . traverse . _2 . _1) fst

  toApply :: Word :* CoreWord :* ShouldElide :* a -> WordShouldElide
  toApply (Word _ _ _ a b _ c d e f, (cw, (se, _))) = WordShouldElide cw a b se c d e f

  to4 :: Validation a [ctx :* Word :* CoreWord :* ShouldElide :* InitialAspiration]
    -> Validation a [ctx :* WordShouldElide]
  to4 = over (_Success . traverse . _2) toApply

  from = from4 . from3 . from2 . from1

  from1 = over (traverse . _2) (\(w, ia) -> w :^ wordShouldElideCoreWord w :^ wordShouldElideShouldElide w :^ ia)
    . addAspirationContext (coreWordAspiration . wordShouldElideCoreWord)
  from2 = traverseWithItemContext . _2 $ roundFrom Round.shouldElide
  from3 = over (_Failure . traverse . _2 . _1) fst
  from4 = over (_Success . traverse . _2) fromApply
  fromApply :: WordShouldElide :* CoreWord :* Elision :* a -> Word
  fromApply (WordShouldElide _ a b _ c d e i, (CoreWord f g h, (el, _))) = Word f g h a b el c d e i
