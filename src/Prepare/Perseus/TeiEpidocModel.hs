module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)

data TeiHeader = TeiHeader
  { teiHeaderType :: Text
  }
  deriving (Show)

data Tei = Tei TeiHeader
  deriving (Show)
