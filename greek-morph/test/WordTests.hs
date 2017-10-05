{-# LANGUAGE QuasiQuotes #-}

module WordTests where

import Test.Framework
import qualified Grammar.Greek.Morph.Phoneme.Round as Round
import Grammar.Greek.Morph.QuasiQuoters
import Grammar.Greek.Morph.Types
import Grammar.Test.Round

coreWordPhonemeRound = testGroup "coreWordPhonemeRound" $ fmap (uncurry $ testRoundId Round.phoneme)
  [coreWordPairs| ἀντί διά κατά ὑπό δέ οὐδέ τοῦτο φυγῆς γλῶττῃ |]
