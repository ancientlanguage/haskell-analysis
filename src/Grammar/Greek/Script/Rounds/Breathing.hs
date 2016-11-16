module Grammar.Greek.Script.Rounds.Breathing where

import Control.Lens (over)
import Data.Either.Validation
import Grammar.Common.Round
import Grammar.Common.Types
import Grammar.Greek.Script.Types

data InvalidBreathing c v a
  = InvalidInitialBreathing ([c] :* v :* a :* Maybe Breathing)
  | InvalidInitialRough ([c] :* v :* a :* Maybe Breathing)
  | InvalidMedialRough (Int :* [c] :* v :* a :* Maybe Breathing)
  | MultipleCrasis [Crasis]
  deriving (Show)

breathing :: forall c v a. RoundFwd [InvalidBreathing c v a]
  ([[c] :* v :* a :* Maybe Breathing])
  ([[c] :* v :* a] :* MarkPreservation :* Crasis :* InitialAspiration)
breathing = makeRoundFwd to from
  where
  to [] = Success $ [] :^ PreserveMarks :^ NoCrasis :^ NoInitialAspiration
  to (x : xs) = case getInitial x of
    Failure e1 -> Failure e1
    Success (mp, (c, asp)) -> case getAllCrasis xs of
      Failure e2 -> Failure e2
      Success cs -> case validateAllCrasis c cs of
        Failure e3 -> Failure e3
        Success c' -> Success $ fmap dropBreathing (x : xs) :^ mp :^ c' :^ asp

  dropBreathing (a, (b, (c, _))) = a :^ b :^ c

  getInitial
    :: [c] :* v :* a :* Maybe Breathing
    -> Validation [InvalidBreathing c v a] (MarkPreservation :* Crasis :* InitialAspiration)
  getInitial ([] :^ _ :^ _ :^ Nothing) = Success $ Unmarked :^ NoCrasis :^ NoInitialAspiration
  getInitial ([] :^ _ :^ _ :^ Just B_Rough) = Success $ PreserveMarks :^ NoCrasis :^ HasInitialAspiration
  getInitial ([] :^ _ :^ _ :^ Just B_Smooth) = Success $ PreserveMarks :^ NoCrasis :^ NoInitialAspiration
  getInitial ((_ : _) :^ _ :^ _ :^ Just B_Smooth) = Success $ PreserveMarks :^ HasCrasis 0 :^ NoInitialAspiration
  getInitial x@((_ : _) :^ _ :^ _ :^ Just B_Rough) = Failure [InvalidInitialRough x]
  getInitial _ = Success $ PreserveMarks :^ NoCrasis :^ NoInitialAspiration

  getCrasis :: Int :* [c] :* v :* a :* Maybe Breathing -> Validation [InvalidBreathing c v a] Crasis
  getCrasis (p :^ _ :^ _ :^ _ :^ Just B_Smooth) = Success $ HasCrasis p
  getCrasis x@(_ :^ _ :^ _ :^ _ :^ Just B_Rough) = Failure [InvalidMedialRough x]
  getCrasis _ = Success NoCrasis

  getAllCrasis :: [[c] :* v :* a :* Maybe Breathing] -> Validation [InvalidBreathing c v a] [Crasis]
  getAllCrasis = over _Success (filter isCrasis) . traverse getCrasis . zip [1..]
    where
    isCrasis (HasCrasis _) = True
    isCrasis NoCrasis = False

  validateAllCrasis :: Crasis -> [Crasis] -> Validation [InvalidBreathing c v s] Crasis
  validateAllCrasis x [] = Success x
  validateAllCrasis NoCrasis [x] = Success x
  validateAllCrasis x xs = Failure [MultipleCrasis (x : xs)]

  addBreathing b (x, (y, z)) = (x, (y, (z, b)))
  applyCrasis p (p', q) | p == p' = addBreathing (Just B_Smooth) q
  applyCrasis _ (_, q) = addBreathing Nothing q

  aspirationToBreathing HasInitialAspiration = Just B_Rough
  aspirationToBreathing NoInitialAspiration = Just B_Smooth

  from ((x@([], _) : xs) :^ PreserveMarks :^ _ :^ asp) = addBreathing (aspirationToBreathing asp) x : fmap (addBreathing Nothing) xs
  from (xs :^ PreserveMarks :^ HasCrasis p :^ _) = fmap (applyCrasis p) . zip [0..] $ xs
  from (xs, _) = fmap (addBreathing Nothing) xs
