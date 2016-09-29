module Prepare.Sblgnt.Parser where

import Prelude hiding (Word)
import Data.Text (Text)
import Prepare.Sblgnt.Model
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml

link :: NodeParser Link
link = (uncurry Link) <$> Xml.elementContentAttr "a" (Xml.attribute "href")

headContent :: NodeParser HeadContent
headContent
  = HeadContentText <$> Xml.content
  <|> HeadContentLink <$> link

headParagraph :: NodeParser HeadParagraph
headParagraph = HeadParagraph <$> Xml.element "p" (some headContent)

headParagraphList :: NodeParser [HeadParagraph]
headParagraphList = some headParagraph

headTitle :: NodeParser [HeadParagraph]
headTitle = Xml.element "title" headParagraphList

license :: NodeParser [HeadParagraph]
license = Xml.element "license" headParagraphList

title :: NodeParser Text
title = Xml.element "title" Xml.content

verse :: NodeParser Verse
verse = uncurry Verse <$> Xml.elementContentAttr "verse-number" (Xml.attribute "id")

surface :: NodeParser Text
surface = Xml.elementContent "w"

prefix :: NodeParser Text
prefix = Xml.elementContent "prefix"

suffix :: NodeParser Text
suffix = Xml.elementContent "suffix"

word :: NodeParser Word
word = Word <$> optional prefix <*> surface <*> suffix

content :: NodeParser Content
content
  = ContentVerse <$> verse
  <|> ContentWord <$> word

markEndText :: NodeParser Text
markEndText = snd <$> Xml.elementContentAttr "mark-end" (Xml.attributeXml "lang")

ending :: NodeParser Ending
ending = Ending <$> markEndText <*> some paragraph

markEnd :: NodeParser MarkEnd
markEnd = MarkEnd <$> markEndText <*> some ending

paragraph :: NodeParser Paragraph
paragraph = Paragraph <$> Xml.element "p" (many content)

book :: NodeParser Book
book = build <$> Xml.elementAttr "book" attributes children
  where
  build (i, (t, ps, me)) = Book i t ps me
  attributes = do
    i <- Xml.attribute "id"
    return i
  children = do
    t <- title
    ps <- some paragraph
    me <- optional markEnd
    return $ (t, ps, me)

sblgnt :: NodeParser Sblgnt
sblgnt = Xml.element "sblgnt" children
  where
  children = do
    t <- headTitle
    l <- license
    bs <- some book
    return $ Sblgnt t l bs
