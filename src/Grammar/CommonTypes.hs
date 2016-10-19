{-# LANGUAGE DeriveGeneric #-}

module Grammar.CommonTypes
  ( (:*)
  , (:+)
  , SourceId(..)
  , Paragraph(..)
  )
  where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Primary as Primary ()

type a :* b = (a, b)
type a :+ b = Either a b

infixr 6 :*
infixr 5 :+

newtype Paragraph = Paragraph { getParagraph :: Int }
  deriving (Eq, Show, Ord, Generic)
instance Serialize Paragraph

data SourceId = SourceId
  { sourceIdGroup :: Text
  , sourceIdSource :: Text
  }
  deriving (Eq, Show, Ord, Generic)
instance Serialize SourceId
