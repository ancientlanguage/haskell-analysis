module Prepare.Source.Output where

import Prelude hiding (Word, last)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Source.Model

data Context = Context
  { contextIndent :: Text
  }

type Output a = Context -> a -> Text

source :: Group -> Output Source
source g ctx (Source sid st sl sc) =
  Text.concat [ prologue, licenseText, contentsText ]
  where
  prologue = tjoin "\n"
    [ moduleLine
    , ""
    , "open import AncientLanguage.Source"
    , ""
    , termType
    , termDecl
    ]
  newCtx = increaseIndent ctx
  moduleLine = tjoin " "
    [ "module"
    , tjoin "." [ "AncientLanguage", groupId g, sid ]
    , "where"
    ]
  termName = idAsTerm sid
  termType = tspaced [ termName, ":", "Source" ]
  termDecl = tspaced [ termName, "=", "source", quoted sid, quoted st ]
  licenseText = onePerLine (\_ -> quoted) newCtx sl
  contentsText = onePerLine content newCtx sc

content :: Output Content
content ctx (ContentMilestone m) = milestone ctx m
content ctx (ContentWord w) = word ctx w

milestone :: Output Milestone
milestone _ MilestoneParagraph = "p"
milestone ctx (MilestoneVerse v) = verse ctx v

verse :: Output Verse
verse _ (Verse cn vn) = tspaced [ "v", num cn, num vn ]

word :: Output Word
word _ (Word Nothing t " ") = tspaced [ "w", quoted t ]
word _ (Word Nothing t s) = tspaced [ "ws", quoted t, quoted s ]
word _ (Word (Just p) t s) = tspaced [ "wp", quoted p, quoted t, quoted s ]


tjoin :: Text -> [Text] -> Text
tjoin t = Text.intercalate t

tspaced :: [Text] -> Text
tspaced = tjoin " "

emptyContext :: Context
emptyContext = Context ""

increaseIndent :: Context -> Context
increaseIndent (Context i) = Context (Text.append "  " i)

asIs :: Output Text
asIs _ t = t

quoted :: Text -> Text
quoted t = Text.concat [ "\"", t , "\"" ]

num :: Int -> Text
num = Text.pack . show

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

idAsTerm :: Text -> Text
idAsTerm t
  | not . Text.null $ t
  , h <- Text.head t
  , Char.isUpper h
  , tl <- Text.tail t
  = Text.cons (Char.toLower h) tl
idAsTerm t = t
