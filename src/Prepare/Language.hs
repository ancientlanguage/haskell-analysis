{-# LANGUAGE DeriveGeneric #-}

module Prepare.Language where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Language
  = Greek
  | Hebrew
  | Latin
  deriving (Show, Generic)
instance Serialize Language
