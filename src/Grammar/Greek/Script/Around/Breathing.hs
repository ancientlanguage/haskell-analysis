module Grammar.Greek.Script.Around.Breathing where

import Data.Either.Validation
import Data.Void
import Grammar.Around
import Grammar.CommonTypes
import Grammar.Greek.Script.Types

data InvalidBreathing c v a
  = InvalidInitialBreathing ([c] :* v :* a :* Maybe Breathing)
  | InvalidMedialBreathing ([c] :* v :* a :* Maybe Breathing)
  deriving (Show)

breathing :: Around [InvalidBreathing c v a] Void
  ([[c] :* v :* a :* Maybe Breathing])
  ([[c] :* v :* a] :* Crasis :* InitialAspiration)
breathing = makeToValidationAround to from
  where
  to [] = Success ([], (NoCrasis, NoInitialAspiration))
  to (x : xs) = (\(x', (v, a)) xs' -> (x' : xs', (v, a))) <$> checkInitial x <*> allMedial xs

  checkInitial (cs, (v, (a, Nothing))) = Success ((cs, (v, a)), (NoCrasis, NoInitialAspiration))
  checkInitial (cs@[], (v, (a, Just B_Smooth))) = Success ((cs, (v, a)), (NoCrasis, NoInitialAspiration))
  checkInitial (cs@[], (v, (a, Just B_Rough))) = Success ((cs, (v, a)), (NoCrasis, HasInitialAspiration))
  checkInitial (cs@(_ : _), (v, (a, Just B_Smooth))) = Success ((cs, (v, a)), (HasCrasis, NoInitialAspiration))
  checkInitial x = Failure [InvalidInitialBreathing x]

  allMedial [] = Success []
  allMedial (x : xs) = pure (:) <*> checkMedial x <*> allMedial xs

  checkMedial (cs, (v, (a, Nothing))) = Success (cs, (v, a))
  checkMedial x@(_, (_, (_, Just _))) = Failure [InvalidMedialBreathing x]

  from ([], (_, _)) = []
  from ((cs, (v, a)) : xs, ci) = (cs, (v, (a, getBreathing cs ci))) : fmap (\(cs, (v, a)) -> (cs, (v, (a, Nothing)))) xs

  getBreathing :: [c] -> (Crasis, InitialAspiration) -> Maybe Breathing
  getBreathing [] (_, HasInitialAspiration) = Just B_Rough
  getBreathing [] (_, NoInitialAspiration) = Just B_Smooth
  getBreathing (_ : _) (HasCrasis, _) = Just B_Smooth
  getBreathing _ _ = Nothing
