module Prepare.Perseus.TeiEpidocUnify where

import Prelude hiding (getContents)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Prepare.Perseus.TeiEpidocHeaderModel as Header
import Prepare.Perseus.TeiEpidocModel
import qualified Primary as Primary

makeId :: Text -> Text -> Text
makeId author title = Text.intercalate "_" [hack author, hack title]
  where
  hack :: Text -> Text
  hack
    = Text.filter (\x -> Char.isLetter x || x == '_')
    . Text.intercalate "_"
    . Text.split (\x -> x == ' ' || x == '.')
    . Text.replace "Greek" ""
    . Text.replace "Machine readable text" ""

getSourceMetadata :: Header.TeiHeader -> Primary.Source
getSourceMetadata h = Primary.Source sid t (Just a) lic []
  where
  sid = makeId a t
  titleStmt = Header.fileDescTitleStmt . Header.teiHeaderFileDesc $ h
  t = Header.titleStmtTitle titleStmt
  a = Header.titleStmtAuthor titleStmt
  lic = []

unifyContent :: Content -> [Primary.Content]
unifyContent _ = []

getSectionContents :: Primary.Division -> Section -> [Primary.Content]
getSectionContents d (Section sn cs)
  = Primary.ContentMilestone (Primary.MilestoneDivision (d { Primary.divisionSection = Just sn }))
  : concatMap unifyContent cs

getChapterContents :: Primary.Division -> Chapter -> [Primary.Content]
getChapterContents d (Chapter cn ss) = concatMap (getSectionContents $ d { Primary.divisionChapter = Just cn}) ss

getBookContents :: Primary.Division -> Book -> [Primary.Content]
getBookContents d (Book bn _ cs) = concatMap (getChapterContents $ d { Primary.divisionBook = Just bn }) cs

emptyDivision :: Primary.Division
emptyDivision = Primary.Division Nothing Nothing Nothing Nothing

getDivisionContents :: Division -> [Primary.Content]
getDivisionContents (DivisionBooks xs) = concatMap (getBookContents emptyDivision) xs
getDivisionContents (DivisionChapters xs) = concatMap (getChapterContents emptyDivision) xs
getDivisionContents (DivisionSections xs) = concatMap (getSectionContents emptyDivision) xs

getEditionContents :: Edition -> [Primary.Content]
getEditionContents (Edition _ _ d) = getDivisionContents d

getBodyContents :: Body -> [Primary.Content]
getBodyContents (BodyEdition e) = getEditionContents e
getBodyContents (BodyDivision d) = getDivisionContents d

getTextContents :: TeiText -> [Primary.Content]
getTextContents = getBodyContents . teiTextBody

unify :: Tei -> Primary.Source
unify (Tei h t) = meta { Primary.sourceContents = getTextContents t }
  where
  meta = getSourceMetadata h
