module Prepare.Tanach.HeaderModel where

import Data.Text (Text)
import Prepare.Tanach.TeiHeaderModel (TeiHeader)

data Note = Note
  { noteCode :: Text
  , noteGccode :: Text
  , noteNote :: Text
  }
  deriving (Show)

data CharInfo = CharInfo
  { charInfoValue :: Text
  , charInfoHexValue :: Text
  , charInfoName :: Text
  , charInfoMccode :: Text
  , charInfoType :: Text
  , charInfoEquivalents :: Text
  , charInfoNotes :: Text
  , charInfoGroup :: Text
  }
  deriving (Show)

data Coding = Coding
  { codingCharInfos :: [CharInfo]
  , codingSpecialChars :: [CharInfo]
  }
  deriving (Show)

data Header = Header
  { headerTeiHeader :: TeiHeader
  , headerCoding :: Coding
  , headerNotes :: [Note]
  }
  deriving (Show)
