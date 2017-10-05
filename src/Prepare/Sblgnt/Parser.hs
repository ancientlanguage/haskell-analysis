{-# LANGUAGE TypeFamilies #-}

module Prepare.Sblgnt.Parser where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Sblgnt.Model
import Prepare.Xml.Parser (NodeParser, (<|>), many, some, optional)
import qualified Prepare.Xml.Parser as Xml
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Lexer as MP
import qualified Text.Megaparsec.Prim as MP

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

verseId
  :: (MP.MonadParsec e s m, MP.Token s ~ Char)
  => m (Text, Integer, Integer)
verseId = do
  bn <- optional (some MP.digitChar <* MP.spaceChar)
  bt <- some MP.letterChar
  _ <- MP.spaceChar
  cn <- MP.integer
  _ <- MP.char ':'
  vn <- MP.integer
  _ <- MP.eof
  return (Text.pack (makeBook bn bt), cn, vn)
  where
    makeBook Nothing t = t
    makeBook (Just bn) t = bn ++ " " ++ t

verse :: NodeParser Verse
verse = do
  (vid, vtext) <- Xml.elementContentAttr "verse-number" (Xml.attribute "id")
  (b, cn, vn) <- Xml.parseNested "verse-number id" verseId vid
  return $ Verse b cn vn vtext

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
