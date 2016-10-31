module Main where

import Control.Lens (over, _1, _2, _Left, toListOf, view)
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either.Validation
import Options.Applicative hiding (Failure, Success)

import Grammar.Around
import Grammar.CommonTypes
import qualified Grammar.Greek.Script.Around as Around
import qualified Grammar.Greek.Stage as Stage
import Grammar.Greek.Script.Types
import Grammar.Prepare
import Grammar.Pretty
import Grammar.Serialize
import qualified Primary

data Options = Options
  { optCommand :: Command
  , optResults :: Int
  , optMatch :: String
  }

resultsParser :: Parser Int
resultsParser = option auto
  ( long "results"
  <> short 'r'
  <> value 10
  <> metavar "R"
  <> help "Output the first R results or 0 for all"
  )

matchParser :: Parser String
matchParser = strOption
  ( long "match"
  <> short 'm'
  <> value ""
  <> metavar "M"
  <> help "Show output whose value matches M"
  )

data Command
  = Words
  | Elision
  | LetterMarks
  | Marks
  | LetterSyllabicMark
  | VowelMarks
  | VowelMarkGroups
  | Crasis
  | MarkPreservation
  | AccentReverseIndex
  | AccentReverseIndexPunctuation

options :: Parser Options
options = subparser
  ( command "sources"
    ( info
      (pure $ Options Words 0 "")
      (progDesc "Show info for primary sources" )
    )
  <> commandQuery "elision" "Show elision" Elision
  <> commandQuery "marks" "Show mark combos" Marks
  <> commandQuery "letter-marks" "Show letter/mark combos" LetterMarks
  <> commandQuery "letter-syllabic-mark" "Show letter/syllabic mark combos" LetterSyllabicMark
  <> commandQuery "vowel-marks" "Show vowel mark combos" VowelMarks
  <> commandQuery "vowel-mark-groups" "Show grouped vowel mark combos" VowelMarkGroups
  <> commandQuery "crasis" "Show crasis" Crasis
  <> commandQuery "mark-preservation" "Show unmarked/marked words" MarkPreservation
  <> commandQuery "accent-reverse-index" "Show aceents with reverse syllable index" AccentReverseIndex
  <> commandQuery "accent-reverse-index-punctuation" "Show aceents with reverse syllable index and word punctuation" AccentReverseIndexPunctuation
  )
  where
  commandQuery n d c = command n
    ( info
      (pure Options <*> pure c <*> resultsParser <*> matchParser)
      (progDesc d)
    )

showWordCounts :: [Primary.Group] -> IO ()
showWordCounts x = mapM_ showGroup x
  where
  showGroup g = mapM_ (showSource (Primary.groupId g)) (Primary.groupSources g)
  showSource g s = Text.putStrLn $ Text.intercalate " "
    [ g
    , Primary.sourceId s
    , "—"
    , textShow . length . filter filterWords $ Primary.sourceContents s
    , "words"
    ]
  filterWords (Primary.ContentWord w) = True
  filterWords _ = False

groupPairs :: Ord k => [k :* v] -> [k :* [v]]
groupPairs = Map.assocs . foldr go Map.empty
  where
  go (k, v) m = case Map.lookup k m of
    Just vs -> Map.insert k (v : vs) m
    Nothing -> Map.insert k [v] m

type MilestoneCtx = Milestone :* Text :* [Text] :* [Text]

queryStage
  :: (Show e1, Ord c, Show c)
  => Around
    (MilestoneCtx :* e1)
    e2
    [MilestoneCtx :* (String :* HasWordPunctuation)]
    [b]
  -> (b -> [c])
  -> Int
  -> String
  -> [Primary.Group]
  -> IO ()
queryStage stg f rc keyMatch gs = showKeyValues . fmap ((over (traverse . _2) concat) . groupPairs . concat) . mapM goSource $ prepareGroups gs
  where
  addCtx
    :: Int
    -> [Milestone :* Primary.Word]
    -> [MilestoneCtx :* String :* HasWordPunctuation]
  addCtx n xs = zipWith3 addContextZip xs lefts rights
    where
    addContextZip (m, w) ls rs = ((m, (fullWordText w, (ls, rs))), basicWord w)

    lefts = leftContext n (onlyText xs)
    rights = rightContext n (onlyText xs)

    fullWordText :: Primary.Word -> Text
    fullWordText (Primary.Word p t s) = Text.concat [p, t, s]

    basicWord :: Primary.Word -> String :* HasWordPunctuation
    basicWord (Primary.Word p t s) = (Text.unpack t, Stage.suffixHasPunctuation s)

    onlyText = fmap (fullWordText . snd)
    rightContext n = snd . foldr go ([], [])
      where
      go x (ctx, xs) = (take n (x : ctx), ctx : xs)
    leftContext n = reverse . fmap reverse . snd . foldl' go ([], [])
      where
      go (ctx, xs) x = (take n (x : ctx), ctx : xs)

  goSource (SourceId g s, ms) = case aroundTo stg . addCtx 5 $ ms of
    Failure es -> do
      _ <- Text.putStrLn $ Text.intercalate " "
        [ g
        , s
        , "Failure: aroundTo --"
        , Text.intercalate "\n" $ fmap prettyMilestoned $ over (traverse . _1) fst es
        ]
      return []
    Success y -> return $ prepareItems (\(x1,x2,x3,x4) -> (Text.concat [ "  ", g , " ", s, " " ] `Text.append` x1,x2,x3,x4)) y

  showAllResults = rc < 0

  showKeyValues xs = do
    case showAllResults of
      True -> putStrLn "Showing summary with all results"
      False -> case rc == 0 of
        True -> putStrLn "Showing summary only"
        False -> putStrLn $ "Showing summary with the first " ++ show rc ++ " results"
    xs' <- xs
    mapM_ skv (filterKeyMatches xs')
    where
    filterKeyMatches = filter (\(k, _) -> null keyMatch || show k == keyMatch)
    skv (k, vs) = do
      _ <- putStrLn $ show k ++ " " ++ show (length vs)
      _ <- mapM_ Text.putStrLn . alignColumns . takeResults $ vs
      if rc /= 0
      then putStrLn ""
      else return ()

  alignColumns :: [(Text, Text, Text, Text)] -> [Text]
  alignColumns xs = fmap (padCombine (maxes xs)) xs
    where
    padCombine (l1,l2,l3,l4) (x1,x2,x3,x4) = Text.intercalate " " $
      [ Text.append x1 (pad x1 l1)
      , Text.concat [pad x2 l2, x2, "  "]
      , Text.append x3 (pad x3 l3)
      , Text.append x4 (pad x4 l4)
      ]
    pad x n = Text.replicate (n - (baseLength x)) " "
    maxes = foldr go (0,0,0,0)
      where
      go (x1,x2,x3,x4) (l1,l2,l3,l4) =
        (max (baseLength x1) l1,max (baseLength x2) l2,max (baseLength x3) l3,max (baseLength x4) l4)
    baseLength = Text.length . Text.filter (not . Char.isMark)

  prepareItems addPrefix = over (traverse . _2) (fmap addPrefix . goBack) . groupPairs . concatMap (\x -> fmap (\y -> (y, x)) (f x))

  takeResults = case showAllResults of
    True -> id
    False -> take rc

  showItems :: [(Milestone :* Text :* [Text] :* [Text]) :* (String :* HasWordPunctuation)]
    -> [(Text, Text, Text, Text)]
  showItems = fmap prettyMilestoneCtxString . Stage.forgetHasWordPunctuation

  goBack xs = case (aroundFrom stg) xs of
    Success ys -> showItems ys
    Failure _ -> [ ("Failure: aroundFrom","","","") ]

handleGroups :: ([Primary.Group] -> IO ()) -> IO ()
handleGroups f = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> f x

getElision = pure . snd . fst . snd

getLetterMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* [Mark]]
getLetterMarks = fst . fst . fst . snd

getMarks
  :: ctx :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Mark]]
getMarks = over traverse snd . fst . fst . fst . snd

getLetterSyllabicMark
  :: ctx :* ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Letter :* Maybe SyllabicMark]
getLetterSyllabicMark = over traverse (\(l, (_, (_, sm))) -> (l, sm)) . fst . fst . fst . snd

getVowelMarks
  :: ctx
    :* ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark) :+ ConsonantRho ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
getVowelMarks = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getVowelMarkGroups
  :: ctx
    :* ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
      :+ [ConsonantRho]
      ]
      :* Capitalization) :* Elision) :* HasWordPunctuation)
  -> [[Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]]
getVowelMarkGroups = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getCrasis
  :: ctx :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* HasWordPunctuation)
  -> [Crasis]
getCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

getMarkPreservation
  :: ctx :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* HasWordPunctuation)
  -> [MarkPreservation]
getMarkPreservation = toListOf (_2 . _2 . _2 . _1)

toAccentReverseIndex :: [Maybe Accent] -> [(Int, Accent)]
toAccentReverseIndex = onlyAccents . addReverseIndex
  where
  onlyAccents :: [(Int, Maybe Accent)] -> [(Int, Accent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []

getAccentReverseIndexPunctuation
  :: ctxctx :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* HasWordPunctuation)
  -> [[Int :* Accent] :* HasWordPunctuation]
getAccentReverseIndexPunctuation = pure . over _1 goAll . getPair
  where
  goAll = toAccentReverseIndex . getAccents

  getPair
    :: m :* [ a :* b :* Maybe Accent ] :* c :* d :* e :* f :* g :* h :* HasWordPunctuation
    -> [ a :* b :* Maybe Accent ] :* HasWordPunctuation
  getPair x = (view (_2 . _1) x, view (_2 . _2 . _2 . _2 . _2 . _2 . _2 . _2) x)

  getAccents :: [ a :* b :* Maybe Accent ] -> [Maybe Accent]
  getAccents = over traverse (view (_2 . _2))

getAccentReverseIndex
  :: ctx :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* a)
  -> [[Int :* Accent]]
getAccentReverseIndex = pure . toAccentReverseIndex . fmap (view (_2 . _2)) . view (_2 . _1)

runCommand :: Options -> IO ()
runCommand (Options Words _ _) = handleGroups showWordCounts
runCommand (Options Elision rc m) = handleGroups (queryStage Stage.toElision getElision rc m)
runCommand (Options LetterMarks rc m) = handleGroups (queryStage Stage.toMarkGroups getLetterMarks rc m)
runCommand (Options Marks rc m) = handleGroups (queryStage Stage.toMarkGroups getMarks rc m)
runCommand (Options LetterSyllabicMark rc m) = handleGroups (queryStage Stage.toMarkSplit getLetterSyllabicMark rc m)
runCommand (Options VowelMarks rc m) = handleGroups (queryStage Stage.toConsonantMarks getVowelMarks rc m)
runCommand (Options VowelMarkGroups rc m) = handleGroups (queryStage Stage.toGroupVowelConsonants getVowelMarkGroups rc m)
runCommand (Options Crasis rc m) = handleGroups (queryStage Stage.toBreathing getCrasis rc m)
runCommand (Options MarkPreservation rc m) = handleGroups (queryStage Stage.toBreathing getMarkPreservation rc m)
runCommand (Options AccentReverseIndex rc m) = handleGroups (queryStage Stage.toBreathing getAccentReverseIndex rc m)
runCommand (Options AccentReverseIndexPunctuation rc m) = handleGroups (queryStage Stage.toBreathing getAccentReverseIndexPunctuation rc m)

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
