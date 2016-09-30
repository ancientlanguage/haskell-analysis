module Prepare.Sblgnt.Unify where

import qualified Prepare.Sblgnt.Model as Sblgnt
import qualified Prepare.Source.Model as Source

unify :: Sblgnt.Sblgnt -> Source.Group
unify _ = Source.Group "Sblgnt" "SBLGNT" []
  [Source.Source "mt" "Mt" [] []]
