{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Grammar.Greek.Script.Stage where

import Prelude hiding (Word)
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

suffixSentence :: Text -> SentenceBoundary
suffixSentence x = go NotSentenceEnd $ Text.unpack x
  where
  go :: SentenceBoundary -> String -> SentenceBoundary
  go s [] = s
  go _ ('.' : _) = SentenceEnd
  go _ (';' : _) = SentenceEnd
  go s (_ : xs) = go s xs

wordWithSentence :: Primary.Word -> String :* SentenceBoundary
wordWithSentence (Primary.Word _ t s) = (Text.unpack t , suffixSentence s)

start
  :: [Primary.Group]
  -> [SourceId :* [Milestone :* String :* SentenceBoundary]]
start = over (traverse . _2 . traverse . _2) wordWithSentence . prepareGroups

travList :: Applicative f => (a -> f b) -> [a] -> f [b]
travList = traverse

forgetSentenceBoundary
  :: [ctx :* [a] :* SentenceBoundary]
  -> [ctx :* [a]]
forgetSentenceBoundary = over (traverse . _2) fst

type AroundContext ctx e1 e2 a b =
  Around
  (ctx :* a :* e1)
  (ctx :* b :* e2)
  [ctx :* a]
  [ctx :* b]

unicodeSymbol :: AroundContext ctx Around.InvalidChar Void
  ([Char] :* SentenceBoundary)
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
unicodeSymbol = Around
  (traverseWithItemContext . _1 . travList $ aroundTo Around.unicodeSymbol)
  (traverseWithItemContext . _1 . travList $ aroundFrom Around.unicodeSymbol)

assocSymbolMark_WordPunctuation :: AroundContext ctx Void Void
  ([Symbol :+ (Mark :+ WordPunctuation)] :* SentenceBoundary)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
assocSymbolMark_WordPunctuation = Around
  (traverseWithItemContext . _1 . travList $ aroundTo Around.sumAssocLeft)
  (traverseWithItemContext . _1 . travList $ aroundFrom Around.sumAssocLeft)

wordPunctuationElision :: AroundContext ctx Around.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
wordPunctuationElision = Around
  (traverseWithItemContext . _1 $ aroundTo Around.wordPunctuationElision)
  (traverseWithItemContext . _1 $ aroundFrom Around.wordPunctuationElision)

symbolLetter :: AroundContext ctx Void Void
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
symbolLetter = Around
  (traverseWithItemContext . _1 . _1 . travList . _Left $ aroundTo Around.symbolLetter)
  (traverseWithItemContext . _1 . _1 . travList . _Left $ aroundFrom Around.symbolLetter)

markGroups :: AroundContext ctx Around.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
markGroups = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.markGroups)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.markGroups)

assocLetterFinal :: AroundContext ctx Void Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
assocLetterFinal = Around
  (traverseWithItemContext . _1 . _1 . travList $ aroundTo around)
  (traverseWithItemContext . _1 . _1 . travList $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to ((l, (c, f)), ms) = ((l, f), (c, ms))
  from ((l, f), (c, ms)) = ((l, (c, f)), ms)

final :: AroundContext ctx [Around.InvalidFinals] Void
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
  (([Letter :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
final = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.final)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.final)

capitalization :: AroundContext ctx Around.InvalidUppercase Void
  (([Letter :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
capitalization = Around
  (traverseWithItemContext . _1 . _1 $ aroundTo Around.capitalization)
  (traverseWithItemContext . _1 . _1 $ aroundFrom Around.capitalization)

markSplit :: AroundContext ctx Around.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
markSplit = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ aroundTo Around.markSplit)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _2 $ aroundFrom Around.markSplit)

letterVowelConsonant :: AroundContext ctx Void Void
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
letterVowelConsonant = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ aroundTo Around.letterVowelConsonant)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _1 $ aroundFrom Around.letterVowelConsonant)

distVowelConsonantMarks :: AroundContext ctx Void Void
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
distVowelConsonantMarks = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundTo Around.distLeftSumOverProd)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundFrom Around.distLeftSumOverProd)

consonantMarks :: AroundContext ctx Around.InvalidConsonantMarks Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
consonantMarks = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ aroundTo Around.consonantMarks)
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Right $ aroundFrom Around.consonantMarks)

groupVowelConsonants :: AroundContext ctx Void Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
groupVowelConsonants = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.groupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.groupSums)

vowelSyllabicMark :: AroundContext ctx Void Void
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe Accent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
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
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
vocalicSyllable = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ aroundTo $ Around.vocalicSyllable (Nothing, Nothing))
  (traverseWithItemContext . _1 . _1 . _1 . travList . _Left $ aroundFrom $ Around.vocalicSyllable (Nothing, Nothing))

swapConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
swapConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundTo Around.swapSum)
  (traverseWithItemContext . _1 . _1 . _1 . travList $ aroundFrom Around.swapSum)

ungroupConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
ungroupConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.ungroupSums)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.ungroupSums)

groupLeftConsonantVocalicSyllables :: AroundContext ctx Void Void
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  (((([ [ConsonantRho] :* (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
groupLeftConsonantVocalicSyllables = Around
  (traverseWithItemContext . _1 . _1 . _1 $ aroundTo Around.groupLeft)
  (traverseWithItemContext . _1 . _1 . _1 $ aroundFrom Around.groupLeft)

breathing :: AroundContext ctx [Around.InvalidBreathing ConsonantRho VocalicSyllable (Maybe Accent)] Void
  (((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent :* Maybe Breathing ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
breathing = Around
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ aroundTo Around.breathing)
  (traverseWithItemContext . _1 . _1 . _1 . _1 $ aroundFrom Around.breathing)

reorderWordProps :: AroundContext ctx Void Void
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* SentenceBoundary)
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
