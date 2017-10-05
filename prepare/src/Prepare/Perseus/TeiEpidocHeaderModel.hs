module Prepare.Perseus.TeiEpidocHeaderModel where

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
  , titleStmtFunder :: Maybe Funder
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
  , imprintPubPlace :: Maybe Text
  }
  deriving (Show)

data Editor = Editor
  { editorRole :: Text
  , editorName :: Text
  }
  deriving (Show)

data Monogr = Monogr
  { monogrAuthor :: Maybe Text
  , monogrTitle :: Maybe Text
  , monogrImprint :: Maybe Imprint
  , monogrEditor :: Maybe Editor
  }
  deriving (Show)

data BiblStruct = BiblStruct
  { biblStructDefault :: Maybe Text
  , biblStructMonogr :: Monogr
  }
  deriving (Show)

data SourceDesc = SourceDesc
  { sourceDescDefault :: Maybe Text
  , sourceDescBiblStruct :: BiblStruct
  }
  deriving (Show)

data FileDesc = FileDesc
  { fileDescTitleStmt :: TitleStmt
  , fileDescExtent :: Maybe Text
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

data Correction = Correction
  { correctionMethod :: Text
  , correctionContent :: Text
  }
  deriving (Show)

data EditorialDecl = EditorialDecl
  { editorialDeclCorrection :: Correction
  }
  deriving (Show)

data EncodingDesc = EncodingDesc
  { encodingDescRefsDeclCts :: [CRefPattern]
  , encodingDescEditorialDecl :: Maybe EditorialDecl
  , encodingDescRefsDeclState :: [RefState]
  }
  deriving (Show)

data Language = Language
  { languageIdent :: Text
  , languageUsage :: Maybe Text
  , languageContent :: Text
  }
  deriving (Show)

data LangUsage = LangUsage
  { langUsageDefault :: Maybe Text
  , langUsageLanguages :: [Language]
  }
  deriving (Show)

data ProfileDesc = ProfileDesc
  { profileDescLangUsage :: LangUsage
  }
  deriving (Show)

data Change = Change
  { changeWhen :: Text
  , changeWho :: Text
  , changeWhat :: Text
  }
  deriving (Show)

data RevisionDesc = RevisionDesc
  { revisionDescChanges :: [Change]
  }
  deriving (Show)

data TeiHeader = TeiHeader
  { teiHeaderType :: Text
  , teiHeaderFileDesc :: FileDesc
  , teiHeaderEncodingDesc :: EncodingDesc
  , teiHeaderProfileDesc :: ProfileDesc
  , teiHeaderRevisionDesc :: RevisionDesc
  }
  deriving (Show)
