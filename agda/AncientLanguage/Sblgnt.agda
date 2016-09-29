{-# OPTIONS --no-eta-equality #-}

module AncientLanguage.Sblgnt where

open import Agda.Builtin.List public
open import Agda.Builtin.String

data Maybe (A : Set) : Set where
  none : Maybe A
  some : A → Maybe A

record Verse : Set where
  constructor verse
  field
    getId : String
    getNumber : String

record Word : Set where
  constructor word
  field
    getPrefix : Maybe String
    getText : String
    getSuffix : Maybe String

data Content : Set where
  verse : Verse → Content
  word : Word → Content

pattern v i n = verse (verse i n)
pattern w t s = word (word none t (some s))
pattern ws t = word (word none t (some " "))
pattern wp p t s = word (word (some p) t (some s))

record Paragraph : Set where
  constructor p
  field
    getContents : List Content

record Ending : Set where
  constructor ending
  field
    getTitle : String
    getParagraphs : List Paragraph

record MarkEnd : Set where
  constructor mark-end
  field
    getTitle : String
    getEndings : List Ending

record Book : Set where
  constructor book
  field
    getId : String
    getTitle : String
    getParagraphs : List Paragraph
    getMarkEnd : Maybe MarkEnd

record Link : Set where
  constructor link
  field
    getHref : String
    getText : String

data HeadContent : Set where
  text : String → HeadContent
  link : Link → HeadContent

pattern a h t = link (link h t)

record HeadParagraph : Set where
  constructor p
  field
    getContents : List HeadContent

record Sblgnt : Set where
  constructor sblgnt
  field
    getTitle : List HeadParagraph
    getLicense : List HeadParagraph
    getBooks : List Book
