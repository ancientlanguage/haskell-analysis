{-# LANGUAGE OverloadedStrings #-}

module Sblgnt where

import Data.Text (Text)
import Text.XML

data Sblgnt = Sblgnt
  { sblgntTitle :: SblgntTitle
  , license :: License
  , books :: [Book]
  }

data Book = Book
  { bookTitle :: Text
  }

data SblgntTitle = SblgntTitle
  {
  }

data License = License
  {
  }

data ParseError =
  UnexpectedName Name
  deriving (Show)

localName :: Text -> Element -> Either ParseError Element
localName t e | elementName e == Name t Nothing Nothing = Right e
localName _ e = Left $ UnexpectedName (elementName e)

parseDocument :: Element -> Either ParseError Sblgnt
parseDocument e = do
  _ <- localName "sblgnt" e
  return $ Sblgnt SblgntTitle License []
