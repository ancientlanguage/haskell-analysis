{-# LANGUAGE DeriveGeneric #-}

module Grammar.CommonTypes
  ( (:*)
  , (:+)
  , SourceId(..)
  , Paragraph(..)
  , Verse(..)
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

data Verse = Verse
  { verseChapter :: Integer
  , verseNumber :: Integer
  }
  deriving (Eq, Show, Ord, Generic)
instance Serialize Verse

addIndex :: [a] -> [(Int, a)]
addIndex = zip [0..]

addReverseIndex :: [a] -> [(Int, a)]
addReverseIndex = snd . foldr go (0, [])
  where
  go x (i, xs) = (i + 1, (i, x) : xs)
