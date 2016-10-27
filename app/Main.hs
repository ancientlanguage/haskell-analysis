module Main where

import Control.Lens (over, _1, _2, _Left, toListOf)
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
  }

sourcesOptions :: Parser Options
sourcesOptions = pure $ Options Words 0

resultCount :: Parser Int
resultCount = option auto
  ( long "results"
  <> short 'r'
  <> value 10
  <> metavar "R"
  <> help "Output the first R results or 0 for all"
  )

data Command
  = Words
  | Elision
  | LetterMarks
  | Marks
  | LetterSyllabicMark
  | VowelMarks
  | VowelMarkGroups

options :: Parser Options
options = subparser
  ( command "sources"
    ( info
      (pure $ Options Words 0)
      (progDesc "Show info for primary sources" )
    )
  <> command "elision"
    ( info
      (pure Options <*> pure Elision <*> resultCount)
      (progDesc "Show elision" )
    )
  <> command "marks"
    ( info
      (pure Options <*> pure Marks <*> resultCount)
      (progDesc "Show mark combos" )
    )
  <> command "letter-marks"
    ( info
      (pure Options <*> pure LetterMarks <*> resultCount)
      (progDesc "Show letter/mark combos" )
    )
  <> command "letter-syllabic-mark"
    ( info
      (pure Options <*> pure LetterSyllabicMark <*> resultCount)
      (progDesc "Show letter/syllabic mark combos" )
    )
  <> command "vowel-marks"
    ( info
      (pure Options <*> pure VowelMarks <*> resultCount)
      (progDesc "Show vowel mark combos" )
    )
  <> command "vowel-mark-groups"
    ( info
      (pure Options <*> pure VowelMarkGroups <*> resultCount)
      (progDesc "Show grouped vowel mark combos" )
    )
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
  -> [Primary.Group]
  -> IO ()
queryStage stg f rc gs = showKeyValues . fmap ((over (traverse . _2) concat) . groupPairs . concat) . mapM goSource $ Stage.start gs
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
      True -> putStrLn "Showing all results"
      False -> case rc == 0 of
        True -> putStrLn "Showing no results"
        False -> putStrLn $ "Showing the first " ++ show rc ++ " results"
    xs' <- xs
    mapM_ skv xs'
    where
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

doQuery x y z = handleGroups (queryStage x y z)

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

runCommand :: Options -> IO ()
runCommand (Options Words _) = handleGroups showWordCounts
runCommand (Options Elision rc) = handleGroups (queryStage Stage.toElision getElision rc)
runCommand (Options LetterMarks rc) = handleGroups (queryStage Stage.toMarkGroups getLetterMarks rc)
runCommand (Options Marks rc) = handleGroups (queryStage Stage.toMarkGroups getMarks rc)
runCommand (Options LetterSyllabicMark rc) = handleGroups (queryStage Stage.toMarkSplit getLetterSyllabicMark rc)
runCommand (Options VowelMarks rc) = handleGroups (queryStage Stage.toConsonantMarks getVowelMarks rc)
runCommand (Options VowelMarkGroups rc) = handleGroups (queryStage Stage.toGroupVowelConsonants getVowelMarkGroups rc)

main :: IO ()
main = execParser opts >>= runCommand
  where
  opts = info (helper <*> options)
    ( fullDesc
    <> progDesc "Query ancient language texts"
    <> header "query" )
