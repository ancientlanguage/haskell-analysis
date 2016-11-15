module Prepare.Perseus.TeiEpidocUnify where

import qualified Prepare.Perseus.TeiEpidocHeaderModel as Header
import Prepare.Perseus.TeiEpidocModel
import qualified Primary as Primary

unify :: Tei -> Primary.Source
