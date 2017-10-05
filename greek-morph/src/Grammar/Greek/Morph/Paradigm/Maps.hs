module Grammar.Greek.Morph.Paradigm.Maps where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Grammar.Common
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Smyth.Nouns.Declension1
import Grammar.Greek.Morph.Smyth.Nouns.Declension2
import Grammar.Greek.Morph.Smyth.Nouns.Declension3

endingFormMap :: Map [Phoneme] [ParadigmForm]
endingFormMap = Map.fromList endingFormGroups

endingFormGroups :: [[Phoneme] :* [ParadigmForm]]
endingFormGroups
  = groupPairs
  $ fmap (\f -> (paradigmEndingEnding . paradigmFormEnding) f :^ f)
  $ declension1Forms
  ++ declension2Forms
  ++ declension3Forms
