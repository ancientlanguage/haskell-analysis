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

data PublicationStmt = PublicationStmt
  { publicationStmtPublisher :: Text
  , publicationStmtPubPlace :: Text
  , publicationStmtAuthority :: Text
  }
  deriving (Show)

data Imprint = Imprint
  { imprintPublisher :: Text
  , imprintDate :: Text
  }
  deriving (Show)

data Monogr = Monogr
  { monogrAuthor :: Text
  , monogrTitle :: Text
  , monogrImprint :: Imprint 
  }
  deriving (Show)

data BiblStruct = BiblStruct
  { biblStructMonogr :: Monogr
  }
  deriving (Show)

data SourceDesc = SourceDesc
  { sourceDescBiblStruct :: BiblStruct
  }
  deriving (Show)

data FileDesc = FileDesc
  { fileDescTitleStmt :: TitleStmt
  , fileDescExtent :: Text
  , fileDescPublicationStmt :: PublicationStmt
  , fileDescSourceDesc :: SourceDesc
  }
  deriving (Show)

data CRefPattern = CRefPattern
  { cRefPatternN :: Text
  , cRefPatternMatchPattern :: Text
  , cRefPatternReplacementPattern :: Text
  , cRefPatternP :: Text
  }
  deriving (Show)

data RefState = RefState
  { refStateUnit :: Text
  , refStateDelim :: Maybe Text
  }
  deriving (Show)

data RefsDecl
  = RefsDeclCts [CRefPattern]
  | RefsDeclState [RefState]
  deriving (Show)

data EncodingDesc = EncodingDesc
  { encodingDescRefsDecls :: [RefsDecl]
  }
  deriving (Show)

data TeiHeader = TeiHeader
  { teiHeaderType :: Text
  , teiHeaderFileDesc :: FileDesc
  }
  deriving (Show)

data Tei = Tei TeiHeader
  deriving (Show)
