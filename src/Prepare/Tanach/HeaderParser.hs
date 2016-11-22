module Prepare.Tanach.HeaderParser where

import Data.Text (Text)
import Prepare.Xml.Parser (NodeParser, many, optional, (<|>))
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Prim as MP
import Prepare.Tanach.HeaderModel

import Prepare.Tanach.TeiHeaderParser (teiHeader)

note :: NodeParser Note
note = Xml.element "note" children
  where
  children
    = pure Note
    <*> Xml.elementContent "code"
    <*> Xml.elementContent "gccode"
    <*> Xml.elementContent "note"

maybeEmptyElement :: Text -> NodeParser Text
maybeEmptyElement n = (MP.try (Xml.elementContent n) <|> (const "" <$> Xml.elementEmpty n <* Xml.whitespace)) 

charInfoChildren :: NodeParser CharInfo
charInfoChildren
  = pure CharInfo
  <*> Xml.elementContent "value"
  <*> Xml.elementContent "hexvalue"
  <*> Xml.elementContent "name"
  <*> Xml.elementContent "mccode"
  <*> Xml.elementContent "type"
  <*> maybeEmptyElement "equivalents"
  <*> maybeEmptyElement "notes"
  <*> Xml.elementContent "group"

specialChar :: NodeParser CharInfo
specialChar = Xml.element "specialchar" charInfoChildren

charInfo :: NodeParser CharInfo
charInfo = Xml.element "char" charInfoChildren

coding :: NodeParser Coding
coding = Xml.element "coding" (Coding <$> many charInfo <*> many specialChar)

header :: NodeParser Header
header = Xml.element "Tanach" children
  where
  children = do
    h <- teiHeader
    (c, n) <- Xml.element "tanach" ((,) <$> coding <*> Xml.element "notes" (many note))
    return $ Header h c n 
