module Prepare.Perseus.TeiEpidocUnify where

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

unify :: Tei -> Primary.Source
unify (Tei h t) = getSourceMetadata h
