module Xml.PositionTypes where

import Data.Text (Text)
import Text.XML.Stream.Parse (PositionRange)
import Text.XML (Name, Prologue, Miscellaneous)

data Node
  = NodeElement Element
  | NodeContent Content
  deriving Show

data Element = Element
  { elementName :: Name
  , elementAttributes :: [(Name, Text)]
  , elementNodes :: [Node]
  , elementPosition :: (PositionRange, PositionRange)
  }
  deriving Show

data Content = Content
  { contentText :: Text
  , contentPosition :: [PositionRange]
  }
  deriving Show

instance Monoid Content where
  mempty = Content "" []
  mappend (Content x1 p1) (Content x2 p2) = Content (mappend x1 x2) (mappend p1 p2)
