{-# LANGUAGE DeriveGeneric #-}

module Grammar.CommonTypes
  ( (:*)
  , (:+)
  , SourceId(..)
  , Paragraph(..)
  , Fwd(..)
  , listToFwd
  , fwdConcatMap
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

data Fwd a
  = F0
  | (:>) a (Fwd a)
  deriving (Eq, Ord, Show, Generic)
infixr 5 :>
instance (Serialize a) => Serialize (Fwd a)
instance Functor Fwd where
  fmap _ F0 = F0
  fmap f (x :> xs) = f x :> fmap f xs
instance Foldable Fwd where
  foldr k z = go
    where
    go F0 = z
    go (y :> ys) = y `k` go ys
instance Traversable Fwd where
  {-# INLINE traverse #-}
  traverse f = foldr cons_f (pure F0)
    where cons_f x ys = (:>) <$> f x <*> ys
listToFwd :: [a] -> Fwd a
listToFwd [] = F0
listToFwd (x : xs) = x :> listToFwd xs
fwdConcatMap :: Foldable t => (a -> Fwd b) -> t a -> Fwd b
fwdConcatMap f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
  where
  build g = g (:>) F0
