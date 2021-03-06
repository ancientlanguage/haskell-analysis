{-# LANGUAGE DeriveGeneric #-}

module Grammar.Common.Types where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Primary as Primary ()

type a :* b = (a, b)
type a :+ b = Either a b

pattern (:^) :: a -> b -> a :* b
pattern x :^ y = (x, y)

infixr 7 :*
infixr 7 :^
infixr 6 :+

data SourceId = SourceId
  { sourceIdGroup :: Text
  , sourceIdSource :: Text
  }
  deriving (Eq, Show, Ord, Generic)
instance Serialize SourceId

data Division = Division
  { divisionBook :: Maybe Integer
  , divisionChapter :: Maybe Integer
  , divisionVerse :: Maybe Integer
  , divisionSection :: Maybe Integer
  , divisionLine :: Maybe Integer
  }
  deriving (Eq, Show, Ord, Generic)
instance Serialize Division

newtype Paragraph = Paragraph { getParagraph :: Int }
  deriving (Eq, Show, Ord, Generic)
instance Serialize Paragraph

type Milestone = Maybe Division :* Maybe Paragraph
