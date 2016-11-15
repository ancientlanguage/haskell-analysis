module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)
import Prepare.Perseus.TeiEpidocHeaderModel

data Tei = Tei
  { teiTeiHeader :: TeiHeader
  }
  deriving (Show)
