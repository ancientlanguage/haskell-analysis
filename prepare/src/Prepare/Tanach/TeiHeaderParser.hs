module Prepare.Tanach.TeiHeaderParser where

import Prelude hiding (Word)
import Data.Text (Text)
import Prepare.Xml.Parser (NodeParser, many, optional)
import qualified Prepare.Xml.Parser as Xml
import Prepare.Tanach.TeiHeaderModel

note :: NodeParser Note
note = Note <$> Xml.elementContent "note"

notesStmt :: NodeParser NotesStmt
notesStmt = NotesStmt <$> Xml.element "notesStmt" (many note)

resp :: NodeParser Resp
resp = Resp <$> Xml.elementContent "resp"

respStmt :: NodeParser RespStmt
respStmt = Xml.element "respStmt"
  ( pure RespStmt
    <*> resp
    <*> many name
  )

edition :: NodeParser Edition
edition = Xml.element "edition"
  ( pure Edition
    <*> Xml.content
    <*> optional (Xml.elementContent "version")
    <*> optional date
  )

editionStmt :: NodeParser EditionStmt
editionStmt = Xml.element "editionStmt" (EditionStmt <$> edition <*> respStmt)

title :: NodeParser Title
title = build <$> Xml.elementContentAttr "title" attributes
  where
  build ((l, t), v) = Title l t v
  attributes = do
    l <- optional (Xml.attribute "level")
    t <- optional (Xml.attribute "type")
    return (l, t)

titleStmt :: NodeParser TitleStmt
titleStmt = Xml.element "titleStmt"
  ( pure TitleStmt
    <*> many title
    <*> many editor
    <*> many respStmt
  )

extent :: NodeParser Extent
extent = Extent <$> Xml.elementContent "extent"

authority :: NodeParser Authority
authority = Xml.element "authority"
  ( pure Authority
    <*> Xml.content
    <*> many name
    <*> date
  )

name :: NodeParser Name
name = uncurry Name <$> Xml.elementContentAttr "name" (optional (Xml.attribute "type"))

distributor :: NodeParser Distributor
distributor = Xml.element "distributor"
  ( pure Distributor
    <*> Xml.content
    <*> many name
  )

availability :: NodeParser Availability
availability = uncurry Availability <$> Xml.elementContentAttr "availability" (Xml.attribute "status")

publicationStmt :: NodeParser PublicationStmt
publicationStmt = Xml.element "publicationStmt"
  ( pure PublicationStmt
    <*> authority
    <*> distributor
    <*> availability
  )

editor :: NodeParser Editor
editor = Editor <$> Xml.elementContent "editor"

imprint :: NodeParser Imprint
imprint = Xml.element "imprint"
  ( pure Imprint
    <*> Xml.elementContent "publisher"
    <*> Xml.elementContent "pubPlace"
    <*> date
  )

idno :: NodeParser Idno
idno = uncurry Idno <$> Xml.elementContentAttr "idno" (optional (Xml.attribute "type"))

biblItem :: NodeParser BiblItem
biblItem = Xml.element "biblItem"
  ( pure BiblItem
    <*> many title
    <*> many editor
    <*> optional respStmt
    <*> optional edition
    <*> imprint
    <*> idno
  )

sourceDesc :: NodeParser SourceDesc
sourceDesc = Xml.element "sourceDesc" (pure SourceDesc <*> many biblItem)

fileDesc :: NodeParser FileDesc
fileDesc = Xml.element "fileDesc"
  ( pure FileDesc
    <*> titleStmt
    <*> editionStmt
    <*> optional extent
    <*> optional publicationStmt
    <*> notesStmt
    <*> optional sourceDesc
  )

encodingDesc :: NodeParser EncodingDesc
encodingDesc = Xml.element "encodingDesc" (pure EncodingDesc)

date :: NodeParser Text
date = Xml.elementContent "date"

creation :: NodeParser Creation
creation = Xml.element "creation"
  ( pure Creation
    <*> Xml.content
    <*> date
  )

language :: NodeParser Language
language = uncurry Language <$> Xml.elementContentAttr "language" (Xml.attribute "ident")

langUsage :: NodeParser LangUsage
langUsage = LangUsage <$> Xml.element "langUsage" language

profileDesc :: NodeParser ProfileDesc
profileDesc = Xml.element "profileDesc"
  ( pure ProfileDesc
    <*> creation
    <*> langUsage
  )

teiHeader :: NodeParser TeiHeader
teiHeader = Xml.element "teiHeader"
  ( pure TeiHeader
    <*> fileDesc
    <*> optional encodingDesc
    <*> optional profileDesc
  )
