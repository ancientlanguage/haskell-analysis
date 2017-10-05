module Grammar.Greek.Morph.Clitic.Round where

import qualified Data.Set as Set
import Grammar.Common
import Grammar.Greek.Morph.Forms.Enclitic
import Grammar.Greek.Morph.Forms.Proclitic
import Grammar.Greek.Morph.Types

clitic :: RoundId CoreWord (CoreWord :* Enclitic :* Proclitic)
clitic = RoundId to from
  where
  to x = x
    :^ (if Set.member x encliticSet then IsEnclitic else NotEnclitic)
    :^ (if Set.member x procliticSet then IsProclitic else NotProclitic)

  encliticSet = Set.fromList . fmap accentedWordToCoreWord $ encliticForms
  procliticSet = Set.fromList procliticForms

  from = fst
