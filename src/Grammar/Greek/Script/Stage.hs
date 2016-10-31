{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Script.Stage where

import Prelude hiding (Word)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Primary
import Grammar.Around (Around(..), (<+>))
import qualified Grammar.Around as Around
import Grammar.CommonTypes
import Grammar.Prepare
import qualified Grammar.Greek.Script.Around as Around
import Grammar.Greek.Script.Types
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

type AroundContext ctx e1 e2 a b =
  Around
  (ctx :* a :* e1)
  (ctx :* b :* e2)
  [ctx :* a]
  [ctx :* b]

unicodeSymbol :: AroundContext ctx Around.InvalidChar Void
  ([Char] :* HasWordPunctuation)
  ([Symbol :+ Mark :+ WordPunctuation] :* HasWordPunctuation)
unicodeSymbol = Around
  (traverseWithItemContext . _1 . travList $ aroundTo Around.unicodeSymbol)
  (traverseWithItemContext . _1 . travList $ aroundFrom Around.unicodeSymbol)

assocSymbolMark_WordPunctuation :: AroundContext ctx Void Void
  ([Symbol :+ (Mark :+ WordPunctuation)] :* HasWordPunctuation)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
assocSymbolMark_WordPunctuation = Around
  (traverseWithItemContext . _1 . travList $ aroundTo Around.sumAssocLeft)
  (traverseWithItemContext . _1 . travList $ aroundFrom Around.sumAssocLeft)

wordPunctuationElision :: AroundContext ctx Around.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* HasWordPunctuation)
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
wordPunctuationElision = Around
  (traverseWithItemContext . _1 $ aroundTo Around.wordPunctuationElision)
  (traverseWithItemContext . _1 $ aroundFrom Around.wordPunctuationElision)

symbolLetter :: AroundContext ctx Void Void
  (([Symbol :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
symbolLetter = Around
  (traverseWithItemContext . _1 . _1 . travList . _Left $ aroundTo Around.symbolLetter)
  (traverseWithItemContext . _1 . _1 . travList . _Left $ aroundFrom Around.symbolLetter)

markGroups :: AroundContext ctx Around.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
markGroups = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.markGroups)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.markGroups)

assocLetterFinal :: AroundContext ctx Void Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
assocLetterFinal = Around
  (traverseWithItemContext . _1 . _1 . travList $ aroundTo around)
  (traverseWithItemContext . _1 . _1 . travList $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to ((l, (c, f)), ms) = ((l, f), (c, ms))
  from ((l, f), (c, ms)) = ((l, (c, f)), ms)

final :: AroundContext ctx [Around.InvalidFinals] Void
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
final = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.final)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.final)

capitalization :: AroundContext ctx Around.InvalidUppercase Void
  (([Letter :* Case :* [Mark]] :* Elision) :* HasWordPunctuation)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
capitalization = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.capitalization)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.capitalization)

markSplit :: AroundContext ctx Around.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
markSplit = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ aroundTo Around.markSplit)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ aroundFrom Around.markSplit)

letterVowelConsonant :: AroundContext ctx Void Void
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
letterVowelConsonant = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ aroundTo Around.letterVowelConsonant)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ aroundFrom Around.letterVowelConsonant)

distVowelConsonantMarks :: AroundContext ctx Void Void
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
distVowelConsonantMarks = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundTo Around.distLeftSumOverProd)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundFrom Around.distLeftSumOverProd)

consonantMarks :: AroundContext ctx Around.InvalidConsonantMarks Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
consonantMarks = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ aroundTo Around.consonantMarks)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ aroundFrom Around.consonantMarks)

groupVowelConsonants :: AroundContext ctx Void Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
groupVowelConsonants = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.groupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.groupSums)

vowelSyllabicMark :: AroundContext ctx Void Void
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe Accent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
vowelSyllabicMark = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ aroundTo around)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left . travList $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to (x, (y, (z, q))) = (x, (q, (y, z)))
  from (x, (q, (y, z))) = (x, (y, (z, q)))

vocalicSyllable :: AroundContext ctx Void Void
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe Accent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
vocalicSyllable = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ aroundTo $ Around.vocalicSyllable (Nothing, Nothing))
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ aroundFrom $ Around.vocalicSyllable (Nothing, Nothing))

swapConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
swapConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundTo Around.swapSum)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundFrom Around.swapSum)

ungroupConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
ungroupConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.ungroupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.ungroupSums)

groupLeftConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  (((([ [ConsonantRho] :* (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
groupLeftConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.groupLeft)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.groupLeft)

breathing :: AroundContext ctx [Around.InvalidBreathing ConsonantRho VocalicSyllable (Maybe Accent)] Void
  (((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent :* Maybe Breathing ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
breathing = Around
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ aroundTo Around.breathing)
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ aroundFrom Around.breathing)

reorderWordProps :: AroundContext ctx Void Void
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* HasWordPunctuation)
  ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* HasWordPunctuation)
reorderWordProps = Around
  (traverseWithItemContext $ aroundTo around)
  (traverseWithItemContext $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to (((((x1, (x2, (x3, x4))), x5), x6), x7), x8) = (x1, (x5, (x2, (x3, (x4, (x6, (x7, x8)))))))
  from (x1, (x5, (x2, (x3, (x4, (x6, (x7, x8))))))) = (((((x1, (x2, (x3, x4))), x5), x6), x7), x8)

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
