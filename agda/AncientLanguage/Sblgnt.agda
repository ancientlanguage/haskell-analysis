module AncientLanguage.Sblgnt where

open import Agda.Builtin.List
open import Agda.Builtin.String

data Maybe (A : Set) : Set where
  none : Maybe A
  some : A → Maybe A

record Verse : Set where
  field
    id : String
    number : String

record Word : Set where
  field
    prefix : Maybe String
    text : String
    suffix : Maybe String

data Content : Set where
  verse : Verse → Content
  word : Word → Content

record Paragraph : Set where
  field
    contents : List Content

record Ending : Set where
  field
    title : String
    paragraphs : List Paragraph

record MarkEnd : Set where
  field
    title : String
    endings : List Ending

record Book : Set where
  field
    id : String
    title : String
    paragraphs : List Paragraph
    markEnd : Maybe MarkEnd

record Body : Set where
  field
    books : List Book

record Link : Set where
  field
    href : String
    text : String

data HeadContent : Set where
  text : String → HeadContent
  link : Link → HeadContent

record HeadParagraph : Set where
  field
    contents : List HeadContent

record Head : Set where
  field
    title : List HeadParagraph
    license : List HeadParagraph

record Sblgnt : Set where
  field
    head : Head
    body : Body
