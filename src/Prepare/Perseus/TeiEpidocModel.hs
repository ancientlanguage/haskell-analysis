module Prepare.Perseus.TeiEpidocModel where

import Data.Text (Text)

data RespStmt = RespStmt
  { respStmtResp :: Text
  , respStmtNames :: [Text]
  }
  deriving (Show)

data Funder = Funder
  { funderN :: Text
  , funderContent :: Text
  }
  deriving (Show)

data TitleStmt = TitleStmt
  { titleStmtTitle :: Text
  , titleStmtAuthor :: Text
  , titleStmtSponsor :: Text
  , titleStmtPrincipal :: Text
  , titleStmtRespStmt :: RespStmt
  , titleStmtFunder :: Funder
  }
  deriving (Show)

data FileDesc = FileDesc
  { fileDescTitleStmt :: TitleStmt
  }
  deriving (Show)

data TeiHeader = TeiHeader
  { teiHeaderType :: Text
  , teiHeaderFileDesc :: FileDesc
  }
  deriving (Show)

data Tei = Tei TeiHeader
  deriving (Show)
