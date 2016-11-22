module Prepare.Tanach.TanachParser where

import Prelude hiding (Word)
import Prepare.Xml.Parser (NodeParser, many, optional, (<|>))
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP
import Prepare.Tanach.TanachModel
import Data.XML.Types (Name(..))

import Prepare.Tanach.TeiHeaderParser (teiHeader)
import Prepare.Tanach.HeaderParser (notes)
import Prepare.Tanach.IndexParser (name, verseCount)

wordContent :: NodeParser WordContent
wordContent
  = MP.try (WordText <$> Xml.content)
  <|> (WordX <$> Xml.elementContent "x")

word :: NodeParser Word
word = Word <$> Xml.element "w" (many wordContent)

milestone :: NodeParser Milestone
milestone
  = MP.try (const MilestonePe <$>  Xml.elementEmpty "pe" <* Xml.whitespace)
  <|> (const MilestoneSamekh <$>  Xml.elementEmpty "samekh" <* Xml.whitespace)

verseContent :: NodeParser VerseContent
verseContent
  = MP.try (VerseWord <$> word)
  <|> (VerseMilestone <$> milestone)
  <|> (VerseWordK <$> Xml.elementContent "k")
  <|> (VerseWordQ <$> Xml.elementContent "q")

verse :: NodeParser Verse
verse = uncurry Verse <$> Xml.elementAttr "v" attributes (many verseContent)
  where
  attributes = Xml.attribute "n" >>= Xml.parseNested "verse number" MP.integer

chapter :: NodeParser Chapter
chapter = build <$> Xml.elementAttr "c" attributes children
  where
  build (cn, (vs, vc)) = Chapter cn vs vc
  attributes = Xml.attribute "n" >>= Xml.parseNested "chapter number" MP.integer
  children
    = pure (,)
    <*> many verse
    <*> verseCount

book :: NodeParser Book
book = Xml.element "book"
  ( pure Book
    <*> name
    <*> many chapter
    <*> verseCount
    <*> (Xml.elementContent "cs" >>= Xml.parseNested "chapter count" MP.integer)
  )

tanach :: NodeParser Tanach
tanach = snd <$> Xml.elementAttr "Tanach" attributes children
  where
  attributes = Xml.attributeNS (Name "noNamespaceSchemaLocation" (Just "http://www.w3.org/2001/XMLSchema-instance") (Just "nsi"))
  children
    = pure Tanach
    <*> teiHeader
    <*> (Xml.element "tanach" book)
    <*> notes
