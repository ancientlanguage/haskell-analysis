module Grammar.Greek.Morph.ShouldElide.Round where

import Control.Lens (over)
import Data.Either.Validation
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Grammar.Common
import Grammar.Greek.Morph.Aspirate
import Grammar.Greek.Morph.Forms.Eliding
import Grammar.Greek.Morph.Types
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word

data InvalidElisionForm =
  InvalidElisionForm (CoreWord :* Elision :* InitialAspiration)
  deriving (Show)
data InvalidElisionCandidate =
  InvalidElisionCandidate (CoreWord :* ShouldElide :* InitialAspiration)
  deriving (Show)

elide
  :: CoreWord :* InitialAspiration
  -> CoreWord :* InitialAspiration
elide (CoreWord asp ss [] :^ nasp) | ((Syllable fc _) : rss) <- reverse ss = CoreWord asp (reverse rss) (f fc) :^ nasp
  where
  f = case nasp of
    HasInitialAspiration -> aspirateList
    NoInitialAspiration -> id
elide x = x

reverseElisionForms :: Map
  (CoreWord :* InitialAspiration)
  (CoreWord :* InitialAspiration)
reverseElisionForms = Map.fromList $ do
  ws <- elidingForms
  nasp <- [ NoInitialAspiration, HasInitialAspiration ]
  let form = ws :^ nasp
  return (elide form, form)

shouldElide :: Round InvalidElisionForm InvalidElisionCandidate
  (CoreWord :* Elision :* InitialAspiration)
  (CoreWord :* ShouldElide :* InitialAspiration)
shouldElide = Round (over _Failure pure . to) (over _Failure pure . from)
  where
  to (w :^ IsElided :^ nasp)
    | Just (w' :^ nasp') <- Map.lookup (w :^ nasp) reverseElisionForms
    = Success $ w' :^ ShouldElide :^ nasp'
  to (w :^ NotElided :^ nasp) = Success $ w :^ ShouldNotElide :^ nasp
  to x = Failure $ InvalidElisionForm x

  from (w :^ ShouldElide :^ nasp) =
    let w' :^ nasp' = elide $ w :^ nasp
    in Success $ w' :^ IsElided :^ nasp'
  from (w :^ ShouldNotElide :^ nasp) = Success $ w :^ NotElided :^ nasp
  from x = Failure $ InvalidElisionCandidate x
