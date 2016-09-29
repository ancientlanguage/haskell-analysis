module Prepare.Sblgnt.Output where

import Prelude hiding (Word, last)
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Sblgnt.Model

data Context = Context
  { contextIndent :: Text
  }

type Output a = Context -> a -> Text

emptyContext :: Context
emptyContext = Context ""

increaseIndent :: Context -> Context
increaseIndent (Context i) = Context (Text.append "  " i)

sblgnt :: Output Sblgnt
sblgnt ctx (Sblgnt ts ls bs) = Text.append prologue $
  Text.concat [ titleText, licenseText, booksText ]
  where
  prologue = Text.intercalate "\n"
    [ "module AncientLanguage.SblgntSample where"
    , ""
    , "open import AncientLanguage.Sblgnt"
    , ""
    , "sblgntTerm : Sblgnt"
    , "sblgntTerm = sblgnt"
    ]
  newCtx = increaseIndent ctx
  titleText = onePerLine headParagraph newCtx ts
  licenseText = onePerLine headParagraph newCtx ls
  booksText = onePerLine book newCtx (take 1 bs)

headParagraph :: Output HeadParagraph
headParagraph ctx (HeadParagraph cs) =
  Text.append "p" $ spaced headContent ctx cs

text :: Text -> Text
text t = Text.concat [ "\"", t , "\"" ]

headContent :: Output HeadContent
headContent _ (HeadContentText t) =
  Text.intercalate " " [ "text", text t ]
headContent _ (HeadContentLink (Link h t)) =
  Text.intercalate " " [ "a", text h, text t ]

book :: Output Book
book ctx (Book i t ps _) = Text.concat
  [ (Text.intercalate " " [ "book", text i, text t ])
  , onePerLine paragraph (increaseIndent ctx) ps
  , " none"
  ]

paragraph :: Output Paragraph
paragraph ctx (Paragraph cs) =
  Text.append "p" $ onePerLine content (increaseIndent ctx) cs

content :: Output Content
content ctx (ContentVerse v) = verse ctx v
content ctx (ContentWord w) = word ctx w

verse :: Output Verse
verse _ (Verse i n) = Text.intercalate " " [ "v", text i, text n ]

word :: Output Word
word _ (Word Nothing t " ") = Text.intercalate " " [ "w", text t ]
word _ (Word Nothing t s) = Text.intercalate " " [ "ws", text t, text s ]
word _ (Word (Just p) t s) = Text.intercalate " " [ "wp", text p, text t, text s ]

newline :: Context -> Text
newline ctx = Text.append "\n" (contextIndent ctx)

join :: (Context -> Text) -> Output a -> Output [a]
join g _ ctx [] = Text.append (g ctx) "[]"
join g f ctx (x : xs) =
  Text.concat . fmap (Text.append (g ctx)) $ first : middles ++ [last]
  where
  first = Text.append "( " (f ctx x)
  middles = fmap (Text.append "∷ " . f ctx) xs
  last = "∷ [] )"

onePerLine :: Output a -> Output [a]
onePerLine = join newline

spaced :: Output a -> Output [a]
spaced = join (const " ")
