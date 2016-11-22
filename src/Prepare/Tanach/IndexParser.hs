module Prepare.Tanach.IndexParser where

import Prelude hiding (Word)
import Prepare.Xml.Parser (NodeParser, many)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Lexer as MP
import Prepare.Tanach.IndexModel

import Prepare.Tanach.TeiHeaderParser (teiHeader)

chapter :: NodeParser Chapter
chapter = build <$> Xml.elementAttr "c" attributes children
  where
  build (n, vs) = Chapter n vs
  attributes = Xml.attribute "n"
    >>= Xml.parseNested "chapter number" MP.integer
  children = Xml.elementContent "vs"
    >>= Xml.parseNested "verse count" MP.integer 

name :: NodeParser Name
name = Xml.element "names"
  ( Name
  <$> Xml.elementContent "name"
  <*> Xml.elementContent "abbrev"
  <*> Xml.elementContent "filename"
  )

chapterCount :: NodeParser Integer
chapterCount = Xml.elementContent "cs"
  >>= Xml.parseNested "chapter count" MP.integer

book :: NodeParser Book
book = Xml.element "book"
  ( Book
  <$> name
  <*> many chapter
  <*> chapterCount
  )

index :: NodeParser Index
index = Xml.element "Tanach"
  ( Index
  <$> teiHeader
  <*> Xml.element "tanach" (many book)
  )
