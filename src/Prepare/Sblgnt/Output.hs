module Prepare.Sblgnt.Output where

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
sblgnt ctx (Sblgnt ts ls bs) = Text.append prologue titleText
  where
  prologue = Text.intercalate "\n"
    [ "module AncientLanguage.SblgntSample where"
    , ""
    , "open import AncientLanguage.Sblgnt"
    , ""
    , "sblgntTerm : Sblgnt"
    , "sblgntTerm = sblgnt"
    , ""
    ]
  titleText = onePerLine headParagraph (increaseIndent ctx) ts

headParagraph :: Output HeadParagraph
headParagraph ctx (HeadParagraph cs) = Text.append
  "p "
  (onePerLine headContent (increaseIndent ctx) cs)

text :: Text -> Text
text t = Text.concat [ "\"", t , "\"" ]

headContent :: Output HeadContent
headContent ctx (HeadContentText t) = Text.append "text " (text t)
headContent ctx (HeadContentLink (Link h t)) = Text.intercalate " " [ "a", (text h), (text t) ]

indent :: Context -> Text -> Text
indent c = Text.append (contextIndent c)

onePerLine :: Output a -> Output [a]
onePerLine f ctx [] = "[]"
onePerLine f ctx (x : xs) =
  Text.intercalate between
    [ (Text.intercalate between $ first : middles)
    , last
    ]
  where
  between = Text.append "\n" (contextIndent ctx)
  first = Text.append "( " (f ctx x)
  middles = fmap (Text.append "∷ " . f ctx) xs
  last = "∷ [])"
