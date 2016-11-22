module Prepare.Tanach.TeiHeaderParser where

import Prelude hiding (Word)
import Prepare.Xml.Parser (NodeParser, (<|>), many, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Prim as MP
import Prepare.Tanach.TeiHeaderModel

note :: NodeParser Note
note = Note <$> Xml.elementContent "note"

notesStmt :: NodeParser NotesStmt
notesStmt = NotesStmt <$> Xml.element "notesStmt" note

resp :: NodeParser Resp
resp = Resp <$> Xml.elementContent "resp"

respStmt :: NodeParser RespStmt
respStmt = RespStmt <$> Xml.element "respStmt" resp

edition :: NodeParser Edition
edition = Xml.element "edition"
  (Edition <$> Xml.content <*> Xml.elementContent "version" <*> Xml.elementContent "date")

editionStmt :: NodeParser EditionStmt
editionStmt = Xml.element "editionStmt" (EditionStmt <$> edition <*> respStmt)

title :: NodeParser Title
title = build <$> Xml.elementContentAttr "title" attributes
  where
  build ((l, t), v) = Title l t v
  attributes = do
    l <- Xml.attribute "level"
    t <- Xml.attribute "type"
    return (l, t)

titleStmt :: NodeParser TitleStmt
titleStmt = TitleStmt <$> Xml.element "titleStmt" (many title) 

fileDesc :: NodeParser FileDesc
fileDesc = Xml.element "fileDesc" $
  FileDesc <$> titleStmt <*> editionStmt <*> notesStmt

teiHeader :: NodeParser TeiHeader
teiHeader = TeiHeader <$> Xml.element "teiHeader" fileDesc
