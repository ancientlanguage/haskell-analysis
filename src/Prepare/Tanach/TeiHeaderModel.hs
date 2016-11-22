module Prepare.Tanach.TeiHeaderModel where

import Data.Text (Text)

data Title = Title
  { titleLevel :: Text
  , titleType :: Text
  , titleValue :: Text
  }
  deriving (Show)

data Edition = Edition
  { editionText :: Text
  , editionVersion :: Text
  , editionDate :: Text
  }
  deriving (Show)

data Resp = Resp Text
  deriving (Show)

data RespStmt = RespStmt
  { respStmtResp :: Resp
  }
  deriving (Show)

data EditionStmt = EditionStmt
  { editionStmtEdition :: Edition
  , editionStmtRespStmt :: RespStmt
  }
  deriving (Show)

data TitleStmt = TitleStmt
  { titleStmtTitles :: [Title]
  }
  deriving (Show)

data Note = Note Text
  deriving (Show)

data NotesStmt = NotesStmt
  { notesStmtNotes :: Note
  }
  deriving (Show)

data FileDesc = FileDesc
  { fileDescTitleStmt :: TitleStmt
  , fileDescEditionStmt :: EditionStmt
  , fileDescNotesStmt :: NotesStmt
  }
  deriving (Show)

data TeiHeader = TeiHeader
  { teiHeaderFileDesc :: FileDesc
  }
  deriving (Show)

data Tanach = Tanach
  { tanachTeiHeader :: TeiHeader
  }
  deriving (Show)
