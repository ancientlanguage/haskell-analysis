module Grammar.Greek.Script.Around.MarkSplit where

import Control.Lens (over)
import Data.Either.Validation
import Data.List (sort)
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidMarkCombo
  = MultipleAccents [Accent]
  | MultipleBreathings [Breathing]
  | MultipleSyllabicMark [SyllabicMark]
  deriving (Show)

markSplit :: Around InvalidMarkCombo Void [Mark] (Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
markSplit = Around to (Success . from)
  where
  to = refineAll . start

  refineAll :: [Accent] :* [[Breathing] :* [SyllabicMark]]
    -> Validation [InvalidMarkCombo] (Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
  refineAll (as, ys) = pure (\x y z -> (x, (y, z))) <*> ras <*> rbs <*> rss
    where
    ras = over _Failure (pure . MultipleAccents) $ refine as
    rbs = over _Failure (pure . MultipleBreathings) $ refine $ (concatMap fst) ys
    rss = over _Failure (pure . MultipleSyllabicMark) $ refine $ (concatMap snd) ys

  refine :: [a] -> Validation [a] (Maybe a)
  refine [] = Success Nothing
  refine [x] = Success (Just x)
  refine xs = Failure xs

  start :: [Mark] -> [Accent] :* [[Breathing] :* [SyllabicMark]]
  start = traverse splitMark

  splitMark :: Mark -> [Accent] :* [Breathing] :* [SyllabicMark]
  splitMark m = (markToAccent m, (markToBreathing m, markToSyllabicMark m))

  markToAccent M_Acute = [A_Acute]
  markToAccent M_Grave = [A_Grave]
  markToAccent M_Circumflex = [A_Circumflex]
  markToAccent _ = []

  markToBreathing M_Smooth = [B_Smooth]
  markToBreathing M_Rough = [B_Rough]
  markToBreathing _ = []

  markToSyllabicMark M_IotaSubscript = [S_IotaSubscript]
  markToSyllabicMark M_Diaeresis = [S_Diaeresis]
  markToSyllabicMark _ = []

  from (ma, (mb, ms)) = sort $ fill accentToMark ma ++ fill breathingToMark mb ++ fill syllabicMarkToMark ms

  fill :: (a -> b) -> Maybe a -> [b]
  fill f = maybe [] (pure . f)

  accentToMark A_Acute = M_Acute
  accentToMark A_Grave = M_Grave
  accentToMark A_Circumflex = M_Circumflex

  breathingToMark B_Smooth = M_Smooth
  breathingToMark B_Rough = M_Rough

  syllabicMarkToMark S_IotaSubscript = M_IotaSubscript
  syllabicMarkToMark S_Diaeresis = M_Diaeresis
