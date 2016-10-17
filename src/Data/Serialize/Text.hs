{-# LANGUAGE DeriveGeneric #-}

module Data.Serialize.Text where

import GHC.Generics (Generic)
import Data.Serialize
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

instance Serialize Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get 
