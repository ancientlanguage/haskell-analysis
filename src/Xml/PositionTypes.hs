module Xml.PositionTypes where

import Data.Text (Text)
import Data.Conduit.Attoparsec (PositionRange(..), Position(..))
import Text.XML (Name)

data Node
  = NodeElement Element
  | NodeContent Content
  deriving (Eq, Ord, Show)

emptyPositionRange :: PositionRange
emptyPositionRange = PositionRange e e where e = Position 0 0

getNodePosition :: Node -> PositionRange
getNodePosition (NodeElement e) = fst . elementPosition $ e
getNodePosition (NodeContent c) = case contentPosition c of
  [] -> emptyPositionRange
  (p : _) -> p 

data Element = Element
  { elementName :: Name
  , elementAttributes :: [(Name, Text)]
  , elementNodes :: [Node]
  , elementPosition :: (PositionRange, PositionRange)
  }
  deriving (Eq, Ord, Show)

data Content = Content
  { contentText :: Text
  , contentPosition :: [PositionRange]
  }
  deriving (Eq, Ord, Show)

instance Monoid Content where
  mempty = Content "" []
  mappend (Content x1 p1) (Content x2 p2) = Content (mappend x1 x2) (mappend p1 p2)
