module Prepare.Source.Output where

import Prelude hiding (Word, last)
import qualified Data.Char as Char
import qualified Data.List.Split as Split
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
  prologue = joinText "\n"
    [ moduleDecl
    , ""
    , "open import AncientLanguage.Source"
    , ""
    , termType
    , termDecl
    ]
  newCtx = increaseIndent ctx
  moduleName = joinText "." [ "AncientLanguage", showText (groupLanguage g), "Source", groupId g, sid ] 
  moduleDecl = spacedText [ "module", moduleName, "where" ]
  termName = idAsTerm sid
  termType = spacedText [ termName, ":", "Source" ]
  termDecl = spacedText [ termName, "=", "source", quoted sid, quoted st ]
  licenseText = onePerLine (const quoted) newCtx sl
  contentsText = contentChunkJoin newCtx (chunkByVerse sc)

chunkByVerse :: [Content] -> [[Content]]
chunkByVerse = Split.split . Split.keepDelimsL . Split.whenElt $ isVerse
  where
  isVerse :: Content -> Bool
  isVerse (ContentMilestone (MilestoneVerse _)) = True
  isVerse _ = False

contentChunkJoin :: Output [[Content]]
contentChunkJoin ctx xs = spacedText
  [ joinText "" [ newline ctx, "( join" ]
  , onePerLine contentChunk ctx xs
  , ")"
  ]

contentChunk :: Output [Content]
contentChunk ctx = onePerLine content (increaseIndent ctx)

content :: Output Content
content ctx (ContentMilestone m) = milestone ctx m
content ctx (ContentWord w) = word ctx w

milestone :: Output Milestone
milestone _ MilestoneParagraph = "p"
milestone ctx (MilestoneVerse v) = verse ctx v

verse :: Output Verse
verse _ (Verse cn vn) = spacedText [ "v", num cn, num vn ]

word :: Output Word
word _ (Word Nothing t " ") = spacedText [ "w", quoted t ]
word _ (Word Nothing t s) = spacedText [ "ws", quoted t, quoted s ]
word _ (Word (Just p) t s) = spacedText [ "wp", quoted p, quoted t, quoted s ]


joinText :: Text -> [Text] -> Text
joinText t = Text.intercalate t

spacedText :: [Text] -> Text
spacedText = joinText " "

showText :: Show a => a -> Text
showText = Text.pack . show

emptyContext :: Context
emptyContext = Context ""

increaseIndent :: Context -> Context
increaseIndent (Context i) = Context (Text.append "  " i)

asIs :: Output Text
asIs _ t = t

quoted :: Text -> Text
quoted t = Text.concat [ "\"", t , "\"" ]

num :: (Num a, Show a) => a -> Text
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
