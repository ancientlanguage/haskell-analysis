module QueryStage where

import Control.Lens (over, _1, _2, _Left, toListOf, view, _Just)
import qualified Data.Char as Char
import Data.List (foldl')
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

groupPairs :: Ord k => [k :* v] -> [k :* [v]]
groupPairs = Map.assocs . foldr go Map.empty
  where
  go (k, v) m = case Map.lookup k m of
    Just vs -> Map.insert k (v : vs) m
    Nothing -> Map.insert k [v] m

data QueryOptions = QueryOptions
  { queryResultCount :: Int
  , queryMatch :: String
  }
  deriving (Show)

type MilestoneCtx = Milestone :* Text :* [Text] :* [Text]

queryStage
  :: (Show e1, Ord c, Show c)
  => Around
    (MilestoneCtx :* e1)
    e2
    [MilestoneCtx :* (String :* HasWordPunctuation)]
    [b]
  -> (b -> [c])
  -> QueryOptions
  -> [Primary.Group]
  -> IO ()
queryStage stg f (QueryOptions rc keyMatch) gs = showKeyValues . fmap ((over (traverse . _2) concat) . groupPairs . concat) . mapM goSource $ prepareGroups gs
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
      , Text.concat ["  ", x4, pad x4 l4]
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
