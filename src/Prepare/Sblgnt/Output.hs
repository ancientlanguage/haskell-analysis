module Prepare.Sblgnt.Output where

import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Sblgnt.Model

data Context = Context
  { contextIndent :: Text
  }

type Output a = Context -> a -> Text

increaseIndent :: Context -> Context
increaseIndent (Context i) = Context (Text.append "  " i)

sblgnt :: Output Sblgnt
sblgnt ctx (Sblgnt t l bs) = Text.intercalate "\n"
  [ "module AncientLanguage.SblgntSample where"
  , ""
  , "open import AncientLanguage.Sblgnt"
  , ""
  , "sblgntTerm : Sblgnt"
  , "sblgntTerm = sblgnt"
  ]

indent :: Context -> Text -> Text
indent c = Text.append (contextIndent c)

onePerLine :: Output a -> Output [a]
onePerLine f ctx [] = "[]"
onePerLine f ctx (x : xs) = 
  Text.intercalate
    (Text.append "\n" (contextIndent ctx))
    (first : middles ++ [last])
  where
  first = Text.append "( " (f ctx x)
  middles = fmap (Text.append "∷ " . f ctx) xs
  last = "∷ [])"
