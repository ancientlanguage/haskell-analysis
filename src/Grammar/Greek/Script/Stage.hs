{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Script.Stage where

import Prelude hiding (Word, round)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Common
import qualified Grammar.Greek.Script.Rounds as Rounds
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Control.Lens (over, _1, _2, _Left, _Right)

suffixHasPunctuation :: Text -> HasWordPunctuation
suffixHasPunctuation x | Text.null . Text.filter (not . Char.isSpace) $ x = NoWordPunctuation
suffixHasPunctuation _ = HasWordPunctuation

wordWithSentence :: Primary.Word -> String :* HasWordPunctuation
wordWithSentence (Primary.Word _ t s) = (Text.unpack t , suffixHasPunctuation s)

basicWord :: Primary.Word -> String :* HasWordPunctuation
basicWord (Primary.Word _ t s) = (Text.unpack t, suffixHasPunctuation s)

fullWordText :: Primary.Word -> Text
fullWordText (Primary.Word p t s) = Text.concat [p, t, s]

start
  :: [Primary.Group]
  -> [SourceId :* [Milestone :* String :* HasWordPunctuation]]
start = over (traverse . _2 . traverse . _2) wordWithSentence . prepareGroups

travList :: Applicative f => (a -> f b) -> [a] -> f [b]
travList = traverse

forgetHasWordPunctuation
  :: [ctx :* [a] :* HasWordPunctuation]
  -> [ctx :* [a]]
forgetHasWordPunctuation = over (traverse . _2) fst

unicodeSymbol :: RoundContext ctx Rounds.InvalidChar Void
  ([Char] :* HasWordPunctuation)
  ([Symbol :+ Mark :+ WordPunctuation] :* HasWordPunctuation)
unicodeSymbol = Round
  (traverseWithItemContext . _1 . travList $ roundFwdTo Rounds.unicodeSymbol)
  (traverseWithItemContext . _1 . travList $ liftRoundFwdFrom Rounds.unicodeSymbol)

assocSymbolMark_WordPunctuation :: RoundContext ctx Void Void
  ([Symbol :+ (Mark :+ WordPunctuation)] :* HasWordPunctuation)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
assocSymbolMark_WordPunctuation = Round
  (traverseWithItemContext . _1 . travList $ liftRoundIdTo sumAssocLeft)
  (traverseWithItemContext . _1 . travList $ liftRoundIdFrom sumAssocLeft)

wordPunctuationElision :: RoundContext ctx Rounds.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
wordPunctuationElision = Round
  (traverseWithItemContext . _1 $ roundFwdTo Rounds.wordPunctuationElision)
  (traverseWithItemContext . _1 $ liftRoundFwdFrom Rounds.wordPunctuationElision)

symbolLetter :: RoundContext ctx Void Void
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
symbolLetter = Round
  (traverseWithItemContext . _1 . _1 . travList . _Left $ liftRoundIdTo Rounds.symbolLetter)
  (traverseWithItemContext . _1 . _1 . travList . _Left $ liftRoundIdFrom Rounds.symbolLetter)

markGroups :: RoundContext ctx Rounds.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
markGroups = Round
  (traverseWithItemContext . _1 . _1 $ roundFwdTo Rounds.markGroups)
  (traverseWithItemContext . _1 . _1 $ liftRoundFwdFrom Rounds.markGroups)

assocLetterFinal :: RoundContext ctx Void Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
assocLetterFinal = Round
  (traverseWithItemContext . _1 . _1 . travList $ liftRoundIdTo round)
  (traverseWithItemContext . _1 . _1 . travList $ liftRoundIdFrom round)
  where
  round = RoundId to from
  to ((l, (c, f)), ms) = ((l, f), (c, ms))
  from ((l, f), (c, ms)) = ((l, (c, f)), ms)

final :: RoundContext ctx [Rounds.InvalidFinals] Void
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
final = Round
  (traverseWithItemContext . _1 $ roundFwdTo Rounds.final)
  (traverseWithItemContext . _1 $ liftRoundFwdFrom Rounds.final)

capitalization :: RoundContext ctx Rounds.InvalidUppercase Void
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
capitalization = Round
  (traverseWithItemContext . _1 . _1 $ roundFwdTo Rounds.capitalization)
  (traverseWithItemContext . _1 . _1 $ liftRoundFwdFrom Rounds.capitalization)

markSplit :: RoundContext ctx Rounds.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
markSplit = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ roundFwdTo Rounds.markSplit)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ liftRoundFwdFrom Rounds.markSplit)

letterVowelConsonant :: RoundContext ctx Void Void
  ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([(Vowel :+ Consonant) :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
letterVowelConsonant = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ liftRoundIdTo Rounds.letterVowelConsonant)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ liftRoundIdFrom Rounds.letterVowelConsonant)

distVowelConsonantMarks :: RoundContext ctx Void Void
  ((([(Vowel :+ Consonant) :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
distVowelConsonantMarks = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList $ liftRoundIdTo distLeftSumOverProd)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ liftRoundIdFrom distLeftSumOverProd)

consonantMarks :: RoundContext ctx Rounds.InvalidConsonantMarks Void
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
consonantMarks = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ roundFwdTo Rounds.consonantMarks)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ liftRoundFwdFrom Rounds.consonantMarks)

groupVowelConsonants :: RoundContext ctx Void Void
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
groupVowelConsonants = Round
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdTo groupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdFrom groupSums)

vowelSyllabicMark :: RoundContext ctx Void Void
  ((([ [Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
vowelSyllabicMark = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ liftRoundIdTo round)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ liftRoundIdFrom round)
  where
  round = RoundId to from
  to (x, (y, (z, q))) = (x, (q, (y, z)))
  from (x, (q, (y, z))) = (x, (y, (z, q)))

vocalicSyllableCore :: RoundId
  ([ [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing] :+ [ConsonantRho] ] :* Capitalization)
  ([ ([VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :* DiaeresisConvention) :+ [ConsonantRho] ] :* Capitalization)
vocalicSyllableCore = RoundId to from
  where
  to (Left vs : xs, cap) =
    ( Left (roundIdTo (Rounds.vocalicSyllable cap) vs)
      : over (travList . _Left) (roundIdTo $ Rounds.vocalicSyllable NotCapitalized) xs
    , cap)
  to (xs, cap) = (over (travList . _Left) (roundIdTo $ Rounds.vocalicSyllable NotCapitalized) xs, cap)
  from (Left vs : xs, cap) =
    ( Left (roundIdFrom (Rounds.vocalicSyllable cap) vs)
      : over (travList . _Left) (roundIdFrom $ Rounds.vocalicSyllable NotCapitalized) xs
    , cap)
  from (xs, cap) = (over (travList . _Left) (roundIdFrom $ Rounds.vocalicSyllable NotCapitalized) xs, cap)

vocalicSyllable :: RoundContext ctx Void Void
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ ([VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :* DiaeresisConvention) :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
vocalicSyllable = Round
  (traverseWithItemContext . _1 . _1 $ to)
  (traverseWithItemContext . _1 . _1 $ from)
  where
  to = liftRoundIdTo vocalicSyllableCore
  from = liftRoundIdFrom vocalicSyllableCore

swapConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
swapConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList $ liftRoundIdTo swapSum)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ liftRoundIdFrom swapSum)

ungroupConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
ungroupConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdTo ungroupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdFrom ungroupSums)

groupLeftConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  (((([ [ConsonantRho] :* (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
groupLeftConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdTo groupLeft)
  (traverseWithItemContext . _1 . _1 . _1 $ liftRoundIdFrom groupLeft)

breathing :: RoundContext ctx [Rounds.InvalidBreathing ConsonantRho VocalicSyllable (Maybe ContextualAccent)] Void
  (((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
breathing = Round
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ roundFwdTo Rounds.breathing)
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ liftRoundFwdFrom Rounds.breathing)

reorderWordProps :: RoundContext ctx Void Void
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  (([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation) :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
reorderWordProps = Round
  (traverseWithItemContext $ liftRoundIdTo round)
  (traverseWithItemContext $ liftRoundIdFrom round)
  where
  round = RoundId to from
  to (((((x1, (x2, (x3, x4))), x5), x6), x7), x8) = ((fmap assocLeft x1, x8), (x5, (x2, (x3, (x4, (x6, x7))))))
  from ((x1, x8), (x5, (x2, (x3, (x4, (x6, x7)))))) = (((((fmap assocRight x1, (x2, (x3, x4))), x5), x6), x7), x8)
  assocLeft (x, (y, z)) = ((x, y), z)
  assocRight ((x, y), z) = (x, (y, z))

accent :: RoundContext ctx Rounds.InvalidContextualAccent Rounds.InvalidWordAccent
  (([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
  (([[ConsonantRho] :* VocalicSyllable] :* Maybe WordAccent :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
accent = Round
  (traverseWithItemContext . _1 $ roundTo Rounds.accent)
  (traverseWithItemContext . _1 $ roundFrom Rounds.accent)

word :: RoundContext ctx Void Void
  (([[ConsonantRho] :* VocalicSyllable] :* Maybe WordAccent :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
  Word
word = Round
  (traverseWithItemContext $ liftRoundIdTo round)
  (traverseWithItemContext $ liftRoundIdFrom round)
  where
  round = RoundId to from
  to ((ss, (mwa, hwp)), (fc, (mp, (cr, (ia, (cap, el)))))) = Word ia (toSyllables ss) fc mwa cr el mp cap hwp
  toSyllables = fmap (\(c, v) -> Syllable c v)
  from (Word ia ss fc mwa cr el mp cap hwp) = ((fromSyllables ss, (mwa, hwp)), (fc, (mp, (cr, (ia, (cap, el))))))
  fromSyllables = fmap (\(Syllable c v) -> (c, v))

toElision
  = unicodeSymbol
  <+> assocSymbolMark_WordPunctuation
  <+> wordPunctuationElision

toMarkGroups
  = toElision
  <+> symbolLetter
  <+> markGroups
  <+> assocLetterFinal
  <+> final
  <+> capitalization

toMarkSplit
  = toMarkGroups
  <+> markSplit

toConsonantMarks
  = toMarkSplit
  <+> letterVowelConsonant
  <+> distVowelConsonantMarks
  <+> consonantMarks

toGroupVowelConsonants
  = toConsonantMarks
  <+> groupVowelConsonants

toVocalicSyllable
  = toGroupVowelConsonants
  <+> vowelSyllabicMark
  <+> vocalicSyllable

script = toVocalicSyllable

-- toBreathing
--   = toVocalicSyllable
--   <+> swapConsonantVocalicSyllables
--   <+> ungroupConsonantVocalicSyllables
--   <+> groupLeftConsonantVocalicSyllables
--   <+> breathing
--   <+> reorderWordProps

-- script
--   = toBreathing
--   <+> accent
--   <+> word
