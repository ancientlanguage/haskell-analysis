module Grammar.Greek.Script.Rounds.Breathing where

import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidBreathing c v a
  = InvalidInitialBreathing ([c] :* v :* a :* Maybe Breathing)
  | InvalidMedialBreathing ([c] :* v :* a :* Maybe Breathing)
  deriving (Show)

breathing :: RoundFwd [InvalidBreathing c v a]
  ([[c] :* v :* a :* Maybe Breathing])
  ([[c] :* v :* a] :* MarkPreservation :* Crasis :* InitialAspiration)
breathing = makeRoundFwd to from
  where
  to [] = Success ([], (PreserveMarks, (NoCrasis, NoInitialAspiration)))
  to (x : xs) = (\(x', (v, a)) xs' -> (x' : xs', (v, a))) <$> checkInitial x <*> allMedial xs

  checkInitial (cs@[] :^ v :^ a :^ Nothing) = Success $ (cs :^ v :^ a) :^ Unmarked :^ NoCrasis :^ NoInitialAspiration
  checkInitial (cs@[] :^ v :^ a :^ Just B_Smooth) = Success $ (cs :^ v :^ a) :^ PreserveMarks :^ NoCrasis :^ NoInitialAspiration
  checkInitial (cs@[] :^ v :^ a :^ Just B_Rough) = Success $ (cs :^ v :^ a) :^ PreserveMarks :^ NoCrasis :^ HasInitialAspiration
  checkInitial (cs@(_ : _) :^ v :^ a :^ Nothing) = Success $ (cs :^ v :^ a) :^ PreserveMarks :^ NoCrasis :^ NoInitialAspiration
  checkInitial (cs@(_ : _) :^ v :^ a :^ Just B_Smooth) = Success $ (cs :^ v :^ a) :^ PreserveMarks :^ HasCrasis :^ NoInitialAspiration
  checkInitial x = Failure [InvalidInitialBreathing x]

  allMedial [] = Success []
  allMedial (x : xs) = pure (:) <*> checkMedial x <*> allMedial xs

  checkMedial (cs, (v, (a, Nothing))) = Success (cs, (v, a))
  checkMedial x@(_, (_, (_, Just _))) = Failure [InvalidMedialBreathing x]

  from ([], _) = []
  from ((cs, (v, a)) : xs, ci) = cs :^ v :^ a :^ getBreathing cs ci : fmap (\(cs', (v', a')) -> cs' :^ v' :^ a' :^ Nothing) xs

  getBreathing :: [c] -> (MarkPreservation :* Crasis :* InitialAspiration) -> Maybe Breathing
  getBreathing [] (PreserveMarks :^ _ :^ HasInitialAspiration) = Just B_Rough
  getBreathing [] (PreserveMarks :^ _ :^ NoInitialAspiration) = Just B_Smooth
  getBreathing (_ : _) (PreserveMarks :^ HasCrasis :^ _) = Just B_Smooth
  getBreathing _ _ = Nothing
