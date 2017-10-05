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
import Prepare.Decompose
import Primary

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
  , moduleChildren :: [Module]
  }
  deriving (Show)

flatModules :: Module -> [Module]
flatModules m = m : concatMap flatModules (moduleChildren m)

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

groupModule :: Group -> Module
groupModule (Group gid gl gt gd gs) = Module groupModuleName termName groupContents srcModules
  where
  groupContents = Text.concat [ prologue, descText, contentsText, "\n" ]
  groupModuleName = addModuleName gid (addModuleName (showText gl) sourceTypeModuleName)
  imports = fmap moduleName srcModules
  prologue = joinText "\n"
    [ getModulePrefix groupModuleName (sourceTypeModuleName : imports)
    , termType
    , termDecl
    ]
  ctx = increaseIndent emptyContext
  termName = idAsTerm gid
  termType = spacedText [ termName, ":", "Group" ]
  termDecl = spacedText [ termName, "=", "group", quoted gid, showText gl, quoted gt ]
  descText = onePerLine (const quoted) ctx gd
  contentsText = onePerLine (const id) ctx $ fmap moduleTermName srcModules
  srcModules = fmap (sourceModule groupModuleName) gs

sourceTypeModuleName :: ModuleName
sourceTypeModuleName = ModuleName [ "PrimarySource", "AncientLanguage" ]

sourceModule :: ModuleName -> Source -> Module
sourceModule pm (Source sid st _ sl sc) = srcModule
  where
  srcModule = Module srcModuleName termName srcContents []
  srcContents = Text.concat [ prologue, licenseText, contentsText, "\n" ]
  srcModuleName = addModuleName sid pm
  prologue = joinText "\n"
    [ getModulePrefix srcModuleName [sourceTypeModuleName]
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
  go (ContentMilestone (MilestoneDivision (Division _ (Just c) _ _ _))) _ = Just c
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
milestone _ (MilestoneCard _) = "p"
milestone ctx (MilestoneDivision v) = division ctx v

division :: Output Division
division _ (Division _ (Just cn) (Just vn) _ _) = spacedText [ "v", num cn, num vn ]
division _ (Division _ _ _ _ _) = spacedText [ "v", num (0 :: Int), num (0 :: Int) ]

word :: Output Word
word _ (Word p t s) | Text.null p, Text.null s = spacedText [ "w", quoted (decompose t) ]
word _ (Word p t s) | Text.null p = spacedText [ "ws", quoted (decompose t), quoted (decompose s) ]
word _ (Word p t s) = spacedText [ "wp", quoted (decompose p), quoted (decompose t), quoted (decompose s) ]


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
