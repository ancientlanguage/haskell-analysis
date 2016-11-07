module Grammar.IO.QueryStage where

import Control.Lens (over, _1, _2)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either.Validation

import Grammar.IO.RandomSample (randomSample)
import Grammar.CommonTypes
import Grammar.Contextualize
import Grammar.Prepare
import Grammar.Pretty
import Grammar.Round

data ResultOption
  = Summary
  | All
  | First Int
  | Random Int
  deriving (Read, Show)

data QueryOptions = QueryOptions
  { queryResultOption :: ResultOption
  , queryMatch :: String
  , queryOmit :: String
  }
  deriving (Show)

type MilestoneCtx = Milestone :* Text :* [Text] :* [Text]

addMilestoneCtx
  :: Int
  -> (a -> Text)
  -> [Milestone :* a]
  -> [MilestoneCtx :* a]
addMilestoneCtx n f xs = zipWith merge (contextualize n texts) xs
  where
  texts = fmap (f . snd) xs
  merge t (m, w) = ((m, t), w)

showKeyValues
  :: Show a
  => QueryOptions
  -> ([b] -> [Text])
  -> [(a, [b])]
  -> IO ()
showKeyValues (QueryOptions ro keyMatch omitMatch) alignShow xs = do
  case ro of
    Summary -> putStrLn "Showing summary"
    All -> putStrLn "Showing all results"
    First rc -> putStrLn $ "Showing the first " ++ show rc ++ " results per heading"
    Random rc -> putStrLn $ "Showing " ++ show rc ++ " random results per heading"
  _ <- putStrLn $ show (length xs) ++ " headings\n"
  mapM_ skv (filterKeyMatches xs)
  where
  filterKeyMatches = filter (\(k, _) -> (null keyMatch || show k == keyMatch) && (null omitMatch || show k /= omitMatch))
  skv (k, vs) = do
    _ <- putStrLn $ show k ++ " " ++ show (length vs)
    vs' <- sampleResults vs
    _ <- mapM_ Text.putStrLn . alignShow $ vs'
    case ro of
      Summary -> return ()
      _ -> putStrLn ""

  sampleResults = case ro of
    Summary -> return . const []
    All -> return . id
    First rc -> return . take rc
    Random rc -> randomSample rc

alignResultColumns :: [(Text, Text, Text, Text)] -> [Text]
alignResultColumns xs = fmap (padCombine (maxes xs)) xs
  where
  padCombine (l1,l2,l3,l4) (x1,x2,x3,x4) = Text.intercalate " " $
    [ Text.append x1 (pad x1 l1)
    , Text.concat [pad x2 l2, x2, "  "]
    , Text.append x3 (pad x3 l3)
    , Text.concat ["  ", x4, pad x4 l4]
    ]
  pad x n = Text.replicate (n - (baseLength x)) " "
  maxes = foldr go (0,0,0,0)
    where
    go (x1,x2,x3,x4) (l1,l2,l3,l4) =
      (max (baseLength x1) l1,max (baseLength x2) l2,max (baseLength x3) l3,max (baseLength x4) l4)
  baseLength = Text.length . Text.filter (not . Char.isMark)

queryStageContextWords2
  :: (Show e1, Ord c, Show c)
  => Int
  -> Int
  -> Round
    (MilestoneCtx :* e1)
    e2
    [MilestoneCtx :* s]
    [b]
  -> (b :* [b] :* [b] -> [c])
  -> QueryOptions
  -> (w -> s)
  -> (w -> Text)
  -> ([MilestoneCtx :* s] -> [MilestoneCtx :* String])
  -> [SourceId :* [Milestone :* w]]
  -> IO ()
queryStageContextWords2 queryContextSize resultContextSize stg itemQuery qo wordString wordText forget ws =
  mapM goSource ws
    >>= showKeyValues qo alignResultColumns . over (traverse . _2) concat . groupPairs . concat
  where
  goSource (sid, ms) = continueForward (showSourceId sid) $ roundTo stg . over (traverse . _2) wordString . addMilestoneCtx resultContextSize wordText $ ms
  showSourceId (SourceId g s) = Text.intercalate " " [g, s]

  continueForward sid (Failure es) = do
    _ <- Text.putStrLn $ Text.intercalate " "
      [ sid
      , "Failure: roundTo --"
      , Text.intercalate "\n" $ fmap prettyMilestoned $ over (traverse . _1) fst es
      ]
    return []
  continueForward sid (Success (result :: [b])) = return . over (traverse . _2) (fmap addPrefix . goBack) . groupPairs . itemQueryWithContext $ result
    where
    addPrefix (x1,x2,x3,x4) = (Text.concat [ "  ", sid, " " ] `Text.append` x1,x2,x3,x4)

  itemQueryWithContext = concatMap (\x -> fmap (\y -> (y, fst x)) (itemQuery x)) . contextualize queryContextSize

  showItems = fmap (prettyMilestoneCtx . fst) . forget

  goBack xs = case (roundFrom stg) xs of
    Success ys -> showItems ys
    Failure _ -> [ ("Failure: roundFrom","","","") ]
