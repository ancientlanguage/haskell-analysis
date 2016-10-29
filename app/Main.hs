module Main where

import Control.Lens (over, _1, _2, _Left, toListOf, view)
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

queryStage
  :: (Show e1, Ord c, Show c)
  => Around
    (Milestone :* e1)
    e2
    [Milestone :* (String :* SentenceBoundary)]
    [b]
  -> (b -> [c])
  -> Int
  -> String
  -> [Primary.Group]
  -> IO ()
queryStage stg f rc keyMatch gs = showKeyValues . fmap ((over (traverse . _2) concat) . groupPairs . concat) . mapM goSource $ Stage.start gs
  where
  goSource (SourceId g s, ms) = case (aroundTo stg) ms of
    Failure es -> do
      _ <- Text.putStrLn $ Text.intercalate " "
        [ g
        , s
        , "Failure: aroundTo --"
        , Text.intercalate "\n" $ fmap prettyMilestoned es
        ]
      return []
    Success y -> return $ prepareItems (Text.append (Text.concat [ "  ", g , " ", s, " " ])) y

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
      _ <- mapM_ Text.putStrLn (takeResults vs)
      if rc /= 0
      then putStrLn ""
      else return ()

  prepareItems addPrefix = over (traverse . _2) (fmap addPrefix . goBack) . groupPairs . concatMap (\x -> fmap (\y -> (y, x)) (f x))

  takeResults = case showAllResults of
    True -> id
    False -> take rc

  showItems :: [Milestone :* (String :* SentenceBoundary)] -> [Text]
  showItems = fmap prettyMilestonedString . Stage.forgetSentenceBoundary

  goBack xs = case (aroundFrom stg) xs of
    Success ys -> showItems ys
    Failure _ -> [ "Failure: aroundFrom" ]

handleGroups :: ([Primary.Group] -> IO ()) -> IO ()
handleGroups f = do
  result <- readGroups
  case result of
    Left x -> putStrLn x
    Right x -> f x

getElision = pure . snd . fst . snd

getLetterMarks
  :: Milestone :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
  -> [Letter :* [Mark]]
getLetterMarks = fst . fst . fst . snd

getMarks
  :: Milestone :* ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
  -> [[Mark]]
getMarks = over traverse snd . fst . fst . fst . snd

getLetterSyllabicMark
  :: Milestone :* ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  -> [Letter :* Maybe SyllabicMark]
getLetterSyllabicMark = over traverse (\(l, (_, (_, sm))) -> (l, sm)) . fst . fst . fst . snd

getVowelMarks
  :: Milestone
    :* ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark) :+ ConsonantRho ]
      :* Capitalization) :* Elision) :* SentenceBoundary)
  -> [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
getVowelMarks = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getVowelMarkGroups
  :: Milestone
    :* ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
      :+ [ConsonantRho]
      ]
      :* Capitalization) :* Elision) :* SentenceBoundary)
  -> [[Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]]
getVowelMarkGroups = toListOf (_2 . _1 . _1 . _1 . traverse . _Left)

getCrasis
  :: Milestone :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* SentenceBoundary)
  -> [Crasis]
getCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

getMarkPreservation
  :: Milestone :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* SentenceBoundary)
  -> [MarkPreservation]
getMarkPreservation = toListOf (_2 . _2 . _2 . _1)

getAccentReverseIndex
  :: Milestone :* ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* SentenceBoundary)
  -> [[Int :* Accent] :* SentenceBoundary]
getAccentReverseIndex = pure . over _1 goAll . getPair
  where
  goAll = onlyAccents . addReverseIndex . getAccents

  getPair
    :: m :* [ a :* b :* Maybe Accent ] :* c :* d :* e :* f :* g :* h :* SentenceBoundary
    -> [ a :* b :* Maybe Accent ] :* SentenceBoundary
  getPair x = (view (_2 . _1) x, view (_2 . _2 . _2 . _2 . _2 . _2 . _2 . _2) x)

  getAccents :: [ a :* b :* Maybe Accent ] -> [Maybe Accent]
  getAccents = over traverse (view (_2 . _2))
  addReverseIndex :: [a] -> [(Int, a)]
  addReverseIndex = snd . foldr go (0, [])
    where
    go x (i, xs) = (i + 1, (i, x) : xs)
  onlyAccents :: [(Int, Maybe Accent)] -> [(Int, Accent)]
  onlyAccents = concatMap go
    where
    go (i, Just x) = [(i, x)]
    go _ = []

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

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
