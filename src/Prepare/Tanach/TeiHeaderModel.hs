module Prepare.Tanach.TeiHeaderModel where

import Data.Text (Text)

data Title = Title
  { titleLevel :: Maybe Text
  , titleType :: Maybe Text
  , titleValue :: Text
  }
  deriving (Show)

data Edition = Edition
  { editionText :: Text
  , editionVersion :: Maybe Text
  , editionDate :: Maybe Text
  }
  deriving (Show)

data Resp = Resp Text
  deriving (Show)

data Name = Name
  { namePosition :: Maybe Text
  , nameValue :: Text
  }
  deriving (Show)

data RespStmt = RespStmt
  { respStmtResp :: Resp
  , respStmtNames :: [Name]
  }
  deriving (Show)

data EditionStmt = EditionStmt
  { editionStmtEdition :: Edition
  , editionStmtRespStmt :: RespStmt
  }
  deriving (Show)

data Editor = Editor Text
  deriving (Show)

data TitleStmt = TitleStmt
  { titleStmtTitles :: [Title]
  , titleStmtEditors :: [Editor]
  , titleStmtRespStmts :: [RespStmt]
  }
  deriving (Show)

data Note = Note Text
  deriving (Show)

data NotesStmt = NotesStmt
  { notesStmtNotes :: [Note]
  }
  deriving (Show)

data Extent = Extent Text
  deriving (Show)

data Authority = Authority
  { authorityValue :: Text
  , authorityNames :: [Name]
  , authorityDate :: Text
  }
  deriving (Show)

data Distributor = Distributor
  { distributorValue :: Text
  , distributorNames :: [Name]
  }
  deriving (Show)

data Availability = Availability
  { availabilityStatus :: Text
  , availabilityValue :: Text
  }
  deriving (Show)

data PublicationStmt = PublicationStmt
  { publicationStmtAuthority :: Authority
  , publicationStmtDistributor :: Distributor
  , publicationStmtAvailability :: Availability
  }
  deriving (Show)

data Idno = Idno
  { idnoType :: Maybe Text
  , idnoValue :: Text
  }
  deriving (Show)

data Imprint = Imprint
  { imprintPublisher :: Text
  , imprintPubPlace :: Text
  , imprintData :: Text
  }
  deriving (Show)

data BiblItem = BiblItem
  { biblItemTitles :: [Title]
  , biblItemEditors :: [Editor]
  , biblItemRespStmt :: Maybe RespStmt
  , biblItemEdition :: Maybe Edition
  , biblItemImprint :: Imprint
  , biblItemIdno :: Idno
  }
  deriving (Show)

data SourceDesc = SourceDesc [BiblItem]
  deriving (Show)

data FileDesc = FileDesc
  { fileDescTitleStmt :: TitleStmt
  , fileDescEditionStmt :: EditionStmt
  , fileDescExtent :: Maybe Extent
  , fileDescPublicationStmt :: Maybe PublicationStmt
  , fileDescNotesStmt :: NotesStmt
  , fileDescSourceDesc :: Maybe SourceDesc
  }
  deriving (Show)

data EncodingDesc = EncodingDesc
  deriving (Show)

data Creation = Creation
  { creationValue :: Text
  , creationDate :: Text
  }
  deriving (Show)

data Language = Language
  { languageIdent :: Text
  , languageValue :: Text
  }
  deriving (Show)

data LangUsage = LangUsage
  { langUsageLanguage :: Language
  }
  deriving (Show)

data ProfileDesc = ProfileDesc
  { profileDescCreation :: Creation
  , profileDescLangUsage :: LangUsage
  }
  deriving (Show)

data TeiHeader = TeiHeader
  { teiHeaderFileDesc :: FileDesc
  , teiHeaderEncodingDesc :: Maybe EncodingDesc
  , teiHeaderProfileDesc :: Maybe ProfileDesc
  }
  deriving (Show)

data Tanach = Tanach
  { tanachTeiHeader :: TeiHeader
  }
  deriving (Show)
