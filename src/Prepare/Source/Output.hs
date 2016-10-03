module Prepare.Source.Output where

import Prelude hiding (Word, last)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as FilePath
import qualified System.FilePath as FilePath
import Prepare.Source.Model

data Context = Context
  { contextIndent :: Text
  }

type Output a = Context -> a -> Text

newtype ModuleName = ModuleName { getModuleName :: [Text] }
  deriving (Show)

emptyModuleName :: ModuleName
emptyModuleName = ModuleName []

addModuleName :: Text -> ModuleName -> ModuleName
addModuleName t (ModuleName ps) = ModuleName (t : ps)

dottedModuleName :: ModuleName -> Text
dottedModuleName (ModuleName ps) = joinText "." (reverse ps)

moduleNamePath :: ModuleName -> FilePath
moduleNamePath (ModuleName ps) = FilePath.joinPath . fmap Text.unpack . reverse $ ps

data Module = Module
  { moduleName :: ModuleName
  , moduleTermName :: Text
  , moduleContents :: Text
  }
  deriving (Show)

writeModule :: FilePath -> Module -> IO ()
writeModule dir m = do
  let mp = moduleNamePath (moduleName m)
  let mpf = FilePath.addExtension mp "agda"
  let full = dir FilePath.</> mpf
  let md = FilePath.takeDirectory full
  let createParents = True
  _ <- FilePath.createDirectoryIfMissing createParents md
  _ <- putStrLn ("Writing " ++ full)
  Text.writeFile full (moduleContents m)

groupModules :: ModuleName -> Group -> [Module]
groupModules pm (Group gid gl gt gd gs) = groupModule : srcModules
  where
  groupModule = Module groupModuleName termName groupContents
  groupContents = Text.concat [ prologue, descText, contentsText, "\n" ]
  groupModuleName = addModuleName gid pm
  imports = fmap (flip addModuleName groupModuleName . sourceId) gs
  prologue = joinText "\n"
    [ getModulePrefix groupModuleName imports
    , termType
    , termDecl
    ]
  ctx = increaseIndent emptyContext
  termName = idAsTerm gid
  termType = spacedText [ termName, ":", "Group" ]
  termDecl = spacedText [ termName, "=", "group", quoted gid, showText gl, quoted gt ]
  descText = onePerLine (const quoted) ctx gd
  contentsText = onePerLine (const id) ctx $ fmap (idAsTerm . sourceId) gs
  srcModules = concatMap (sourceModules groupModuleName) gs 

sourceModules :: ModuleName -> Source -> [Module]
sourceModules pm (Source sid st sl sc) = [ srcModule ]
  where
  srcModule = Module srcModuleName termName srcContents
  srcContents = Text.concat [ prologue, licenseText, contentsText, "\n" ]
  srcModuleName = addModuleName sid pm
  typeModuleName = ModuleName [ "Source", "AncientLanguage" ]
  prologue = joinText "\n"
    [ getModulePrefix srcModuleName [typeModuleName]
    , termType
    , termDecl
    ]
  ctx = increaseIndent emptyContext
  termName = idAsTerm sid
  termType = spacedText [ termName, ":", "Source" ]
  termDecl = spacedText [ termName, "=", "source", quoted sid, quoted st ]
  licenseText = onePerLine (const quoted) ctx sl
  contentsText = contentChunkJoin ctx (chunkByMilestone sc)

getModulePrefix :: ModuleName -> [ModuleName] -> Text
getModulePrefix n is =
  joinText "\n"
    [ moduleDecl
    , ""
    , imports
    , ""
    ]
  where
  moduleDecl = spacedText [ "module", dottedModuleName n, "where" ]
  imports = joinText "\n" $ fmap (\x -> spacedText [ "open", "import", dottedModuleName x ]) is

chunkByMilestone :: [Content] -> [[Content]]
chunkByMilestone = Split.split . Split.dropInitBlank . Split.keepDelimsL . Split.condense . Split.whenElt $ isMilestone

isMilestone :: Content -> Bool
isMilestone (ContentMilestone _) = True
isMilestone _ = False

contentChunkJoin :: Output [[Content]]
contentChunkJoin ctx xs = spacedText
  [ joinText "" [ newline ctx, "( join" ]
  , onePerLine contentChunk ctx xs
  , ")"
  ]

getChapter :: [Content] -> Maybe Integer
getChapter = foldr go Nothing
  where
  go (ContentMilestone (MilestoneVerse (Verse c _))) _ = Just c
  go _ r = r

groupByChapter :: [[Content]] -> [[[Content]]]
groupByChapter = List.groupBy f
  where
  f x y = getChapter x == getChapter y

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
