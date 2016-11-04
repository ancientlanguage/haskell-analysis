{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Script.Stage where

import Prelude hiding (Word)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Round (Round(..), (<+>))
import qualified Grammar.Round as Round
import Grammar.CommonTypes
import Grammar.Prepare
import qualified Grammar.Greek.Script.Round as Round
import Grammar.Greek.Script.Types
import Grammar.Greek.Script.Word
import Control.Lens (over, _1, _2, _Left, _Right)

suffixHasPunctuation :: Text -> HasWordPunctuation
suffixHasPunctuation x | Text.null . Text.filter (not . Char.isSpace) $ x = NoWordPunctuation
suffixHasPunctuation _ = HasWordPunctuation

wordWithSentence :: Primary.Word -> String :* HasWordPunctuation
wordWithSentence (Primary.Word _ t s) = (Text.unpack t , suffixHasPunctuation s)

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

type RoundContext ctx e1 e2 a b =
  Round
  (ctx :* a :* e1)
  (ctx :* b :* e2)
  [ctx :* a]
  [ctx :* b]

unicodeSymbol :: RoundContext ctx Round.InvalidChar Void
  ([Char] :* HasWordPunctuation)
  ([Symbol :+ Mark :+ WordPunctuation] :* HasWordPunctuation)
unicodeSymbol = Round
  (traverseWithItemContext . _1 . travList $ roundTo Round.unicodeSymbol)
  (traverseWithItemContext . _1 . travList $ roundFrom Round.unicodeSymbol)

assocSymbolMark_WordPunctuation :: RoundContext ctx Void Void
  ([Symbol :+ (Mark :+ WordPunctuation)] :* HasWordPunctuation)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
assocSymbolMark_WordPunctuation = Round
  (traverseWithItemContext . _1 . travList $ roundTo Round.sumAssocLeft)
  (traverseWithItemContext . _1 . travList $ roundFrom Round.sumAssocLeft)

wordPunctuationElision :: RoundContext ctx Round.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
wordPunctuationElision = Round
  (traverseWithItemContext . _1 $ roundTo Round.wordPunctuationElision)
  (traverseWithItemContext . _1 $ roundFrom Round.wordPunctuationElision)

symbolLetter :: RoundContext ctx Void Void
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
symbolLetter = Round
  (traverseWithItemContext . _1 . _1 . travList . _Left $ roundTo Round.symbolLetter)
  (traverseWithItemContext . _1 . _1 . travList . _Left $ roundFrom Round.symbolLetter)

markGroups :: RoundContext ctx Round.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
markGroups = Round
  (traverseWithItemContext . _1 . _1 $ roundTo Round.markGroups)
  (traverseWithItemContext . _1 . _1 $ roundFrom Round.markGroups)

assocLetterFinal :: RoundContext ctx Void Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
assocLetterFinal = Round
  (traverseWithItemContext . _1 . _1 . travList $ roundTo around)
  (traverseWithItemContext . _1 . _1 . travList $ roundFrom around)
  where
  around = Round.makeIdRound to from
  to ((l, (c, f)), ms) = ((l, f), (c, ms))
  from ((l, f), (c, ms)) = ((l, (c, f)), ms)

final :: RoundContext ctx [Round.InvalidFinals] Void
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
final = Round
  (traverseWithItemContext . _1 . _1 $ roundTo Round.final)
  (traverseWithItemContext . _1 . _1 $ roundFrom Round.final)

capitalization :: RoundContext ctx Round.InvalidUppercase Void
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
capitalization = Round
  (traverseWithItemContext . _1 . _1 $ roundTo Round.capitalization)
  (traverseWithItemContext . _1 . _1 $ roundFrom Round.capitalization)

markSplit :: RoundContext ctx Round.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
markSplit = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ roundTo Round.markSplit)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ roundFrom Round.markSplit)

letterVowelConsonant :: RoundContext ctx Void Void
  ((([Letter :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([(Vowel :+ Consonant) :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
letterVowelConsonant = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ roundTo Round.letterVowelConsonant)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ roundFrom Round.letterVowelConsonant)

distVowelConsonantMarks :: RoundContext ctx Void Void
  ((([(Vowel :+ Consonant) :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
distVowelConsonantMarks = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList $ roundTo Round.distLeftSumOverProd)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ roundFrom Round.distLeftSumOverProd)

consonantMarks :: RoundContext ctx Round.InvalidConsonantMarks Void
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe ContextualAccent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
consonantMarks = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ roundTo Round.consonantMarks)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ roundFrom Round.consonantMarks)

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
  (traverseWithItemContext . _1 . _1 . _1 $ roundTo Round.groupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ roundFrom Round.groupSums)

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
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ roundTo around)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ roundFrom around)
  where
  around = Round.makeIdRound to from
  to (x, (y, (z, q))) = (x, (q, (y, z)))
  from (x, (q, (y, z))) = (x, (y, (z, q)))

vocalicSyllable :: RoundContext ctx Void Void
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe ContextualAccent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
vocalicSyllable = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ roundTo $ Round.vocalicSyllable (Nothing, Nothing))
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ roundFrom $ Round.vocalicSyllable (Nothing, Nothing))

swapConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
swapConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 . travList $ roundTo Round.swapSum)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ roundFrom Round.swapSum)

ungroupConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
ungroupConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 $ roundTo Round.ungroupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ roundFrom Round.ungroupSums)

groupLeftConsonantVocalicSyllables :: RoundContext ctx Void Void
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  (((([ [ConsonantRho] :* (VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing) ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
groupLeftConsonantVocalicSyllables = Round
  (traverseWithItemContext . _1 . _1 . _1 $ roundTo Round.groupLeft)
  (traverseWithItemContext . _1 . _1 . _1 $ roundFrom Round.groupLeft)

breathing :: RoundContext ctx [Round.InvalidBreathing ConsonantRho VocalicSyllable (Maybe ContextualAccent)] Void
  (((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent :* Maybe Breathing ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
breathing = Round
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ roundTo Round.breathing)
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ roundFrom Round.breathing)

reorderWordProps :: RoundContext ctx Void Void
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe ContextualAccent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  (([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation) :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
reorderWordProps = Round
  (traverseWithItemContext $ roundTo around)
  (traverseWithItemContext $ roundFrom around)
  where
  around = Round.makeIdRound to from
  to (((((x1, (x2, (x3, x4))), x5), x6), x7), x8) = ((fmap assocLeft x1, x8), (x5, (x2, (x3, (x4, (x6, x7))))))
  from ((x1, x8), (x5, (x2, (x3, (x4, (x6, x7)))))) = (((((fmap assocRight x1, (x2, (x3, x4))), x5), x6), x7), x8)
  assocLeft (x, (y, z)) = ((x, y), z)
  assocRight ((x, y), z) = (x, (y, z))

accent :: RoundContext ctx Round.InvalidContextualAccent Round.InvalidWordAccent
  (([ ([ConsonantRho] :* VocalicSyllable) :* Maybe ContextualAccent ] :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
  (([[ConsonantRho] :* VocalicSyllable] :* Maybe WordAccent :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
accent = Round
  (traverseWithItemContext . _1 $ roundTo Round.accent)
  (traverseWithItemContext . _1 $ roundFrom Round.accent)

word :: RoundContext ctx Void Void
  (([[ConsonantRho] :* VocalicSyllable] :* Maybe WordAccent :* HasWordPunctuation)
    :* [ConsonantRho] :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision)
  Word
word = Round
  (traverseWithItemContext $ roundTo around)
  (traverseWithItemContext $ roundFrom around)
  where
  around = Round.makeIdRound to from
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

toBreathing
  = toGroupVowelConsonants
  <+> vowelSyllabicMark
  <+> vocalicSyllable
  <+> swapConsonantVocalicSyllables
  <+> ungroupConsonantVocalicSyllables
  <+> groupLeftConsonantVocalicSyllables
  <+> breathing
  <+> reorderWordProps

script
  = toBreathing
  <+> accent
  <+> word
