module Queries where

import Prelude hiding (Word)
import Grammar.Common
import Grammar.IO.QueryStage
import Grammar.Greek.Script.Word
import Grammar.Greek.Morph.Serialize

{-
queryStage
  :: (Show e1, Ord c, Show c)
  => Round
  (MilestoneCtx :* e1)
    e2
    [MilestoneCtx :* Word]
    [b]
  -> (b -> [c])
  -> QueryOptions
  -> IO ()
queryStage a f = queryStageContext 0 a (f . fst)

queryStageContext
  :: (Show e1, Ord c, Show c)
  => Int
  -> Round
    (MilestoneCtx :* e1)
    e2
    [MilestoneCtx :* Word]
    [b]
  -> (b :* [b] :* [b] -> [c])
  -> QueryOptions
  -> IO ()
queryStageContext contextSize stg itemQuery qo = readScript >>= queryStageWithContext contextSize stg itemQuery qo id id id
-}
