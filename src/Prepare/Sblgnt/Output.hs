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
sblgnt ctx (Sblgnt ts ls bs) = Text.append prologue $
  Text.concat [ titleText, licenseText, "\n[]" ]
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

headParagraph :: Output HeadParagraph
headParagraph ctx (HeadParagraph cs) = Text.append
  "p"
  (spaced headContent ctx cs)

text :: Text -> Text
text t = Text.concat [ "\"", t , "\"" ]

headContent :: Output HeadContent
headContent ctx (HeadContentText t) =
  Text.intercalate " " [ "text", (text t) ]
headContent ctx (HeadContentLink (Link h t)) =
  Text.intercalate " " [ "a", (text h), (text t) ]

newline :: Context -> Text
newline ctx = Text.append "\n" (contextIndent ctx)

join :: (Context -> Text) -> Output a -> Output [a]
join g f ctx [] = "[]"
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
