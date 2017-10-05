module Morph where

import qualified Data.Text.Format as Lazy
import qualified Data.Text.Lazy as Lazy
import Grammar.IO.QueryStage
import Grammar.Greek.Morph.Types
import Grammar.Greek.Morph.Paradigm.Types
import Grammar.Greek.Morph.Paradigm.Maps (endingFormGroups)
import Grammar.Greek.Script.Word

showMorph :: Morph -> Lazy.Text
showMorph (Morph a b c d e f g)
  = Lazy.intercalate ", "
  . filter (not . Lazy.null)
  $ [sh a, sh b, sh c, sh d, sh e, sh f, sh g]
  where
  sh Nothing = ""
  sh (Just x) = Lazy.pack . show $ x

showAccent :: WordAccent -> Lazy.Text
showAccent (WordAccent av ap _ _) = Lazy.format "{} {}" (Lazy.Shown av, Lazy.Shown ap)

toTextFullParadigmForm :: ParadigmForm -> Lazy.Text
toTextFullParadigmForm (ParadigmForm k (ParadigmEnding (ParadigmExemplar et _) m acc _)) =
  Lazy.format "  {} -- {} -- {} -- {}" (Lazy.Shown k, Lazy.fromStrict et, showAccent acc, showMorph m)

showMorphForms :: QueryOptions -> IO ()
showMorphForms opt = showKeyValues opt (fmap (Lazy.toStrict . toTextFullParadigmForm)) endingFormGroups
