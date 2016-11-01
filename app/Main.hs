{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Control.Lens (over, _1, _2, _Left, toListOf, view, _Just)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative hiding (Failure, Success)

import QueryStage
import Grammar.CommonTypes
import qualified Grammar.Greek.Stage as Stage
import Grammar.Greek.Script.Types
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
  | ForceAcute
  | AccentPosition
  | FinalConsonants

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
  <> commandQuery "accent-reverse-index-punctuation" "Show accents with reverse syllable index and word punctuation" AccentReverseIndexPunctuation
  <> commandQuery "force-acute" "Show occurences where the accent is forced to be acute" ForceAcute
  <> commandQuery "accent-position" "Show decontextualized accents and positions" AccentPosition
  <> commandQuery "final-consonants" "Show final consonants" FinalConsonants
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
  filterWords (Primary.ContentWord _) = True
  filterWords _ = False

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
  :: ctx :* a :* b :* c :* Crasis :* d
  -> [Crasis]
getCrasis = toListOf (_2 . _2 . _2 . _2 . _1)

getMarkPreservation
  :: ctx :* a :* b :* MarkPreservation :* c
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
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe Accent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision
  -> [[Int :* Accent] :* HasWordPunctuation]
getAccentReverseIndexPunctuation = pure . over _1 goAll . getPair
  where
  goAll = toAccentReverseIndex . getAccents

  getPair
    :: m :* ([ a :* Maybe Accent ] :* HasWordPunctuation) :* b
    -> [ a :* Maybe Accent ] :* HasWordPunctuation
  getPair x = (view (_2 . _1 . _1) x, view (_2 . _1 . _2) x)

  getAccents :: [ a :* Maybe Accent ] -> [Maybe Accent]
  getAccents = over traverse snd

getAccentReverseIndex
  :: ctx :* ([ ([ConsonantRho] :* VocalicSyllable) :* Maybe Accent ] :* a) :* b
  -> [[Int :* Accent]]
getAccentReverseIndex = pure . toAccentReverseIndex . fmap snd . view (_2 . _1 . _1)

getForceAcute
  :: ctx :* (a :* Maybe (b :* c :* ForceAcute :* d) :* e) :* f
  -> [ForceAcute]
getForceAcute = toListOf (_2 . _1 . _2 . _1 . _Just . _2 . _2 . _1)

getAccentPosition
  :: ctx :* (a :* Maybe (WordAccent :* AccentPosition :* b :* c) :* d) :* e
  -> [Maybe (WordAccent :* AccentPosition)]
getAccentPosition = pure . over (_Just . _2) fst . view (_2 . _1 . _2 . _1)

getFinalConsonants
  :: ctx :* a :* [ConsonantRho] :* b
  -> [[ConsonantRho]]
getFinalConsonants = pure . view (_2 . _2 . _1)

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
runCommand (Options ForceAcute rc m) = handleGroups (queryStage Stage.script getForceAcute rc m)
runCommand (Options AccentPosition rc m) = handleGroups (queryStage Stage.script getAccentPosition rc m)
runCommand (Options FinalConsonants rc m) = handleGroups (queryStage Stage.script getFinalConsonants rc m)

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
