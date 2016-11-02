module RandomSample where

import Data.Random.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Source.DevRandom
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL((:<)))

randomSample :: Int -> [a] -> IO [a]
randomSample n = flip runRVar DevRandom . sample n

-- https://github.com/aristidb/random-extras/blob/master/Data/Random/Extras.hs

sample :: Int -> [a] -> RVar [a]
sample m = sampleSeq m . Seq.fromList

(.:) :: (c -> c') -> (a -> b -> c) -> (a -> b -> c')
(.:) = (.).(.)

extractSeq :: Seq.Seq a -> Int -> Maybe (Seq.Seq a, a)
extractSeq s i | Seq.null r = Nothing
               | otherwise  = Just (a >< c, b)
    where (a, r) = Seq.splitAt i s
          (b :< c) = Seq.viewl r

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]

shuffleSeq' :: Seq.Seq a -> [Int] -> [a]
shuffleSeq' = snd .: mapAccumL (fromJust .: extractSeq)

sampleSeq :: Int -> Seq.Seq a -> RVar [a]
sampleSeq m s = do
  samples <- mapM (uniform 0) . take m . backsaw $ Seq.length s
  return (shuffleSeq' s (sort samples))
