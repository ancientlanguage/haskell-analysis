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
  :: [Milestone :* [a] :* SentenceBoundary]
  -> [Milestone :* [a]]
forgetSentenceBoundary = over (traverse . _2) fst

type AroundMilestone e1 e2 a b =
  Around
  (Milestone :* a :* e1)
  (Milestone :* b :* e2)
  [Milestone :* a]
  [Milestone :* b]

unicodeSymbol :: AroundMilestone Around.InvalidChar Void
  ([Char] :* SentenceBoundary)
  ([Symbol :+ Mark :+ WordPunctuation] :* SentenceBoundary)
unicodeSymbol = Around
  (milestoneContext . _1 . travList $ aroundTo Around.unicodeSymbol)
  (milestoneContext . _1 . travList $ aroundFrom Around.unicodeSymbol)

assocSymbolMark_WordPunctuation :: AroundMilestone Void Void
  ([Symbol :+ (Mark :+ WordPunctuation)] :* SentenceBoundary)
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
assocSymbolMark_WordPunctuation = Around
  (milestoneContext . _1 . travList $ aroundTo Around.sumAssocLeft)
  (milestoneContext . _1 . travList $ aroundFrom Around.sumAssocLeft)

wordPunctuationElision :: AroundMilestone Around.InvalidWordPunctuation Void
  ([(Symbol :+ Mark) :+ WordPunctuation] :* SentenceBoundary)
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
wordPunctuationElision = Around
  (milestoneContext . _1 $ aroundTo Around.wordPunctuationElision)
  (milestoneContext . _1 $ aroundFrom Around.wordPunctuationElision)

symbolLetter :: AroundMilestone Void Void
  (([Symbol :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
symbolLetter = Around
  (milestoneContext . _1 . _1 . travList . _Left $ aroundTo Around.symbolLetter)
  (milestoneContext . _1 . _1 . travList . _Left $ aroundFrom Around.symbolLetter)

markGroups :: AroundMilestone Around.InitialMarks Void
  (([(Letter :* Case :* Final) :+ Mark] :* Elision) :* SentenceBoundary)
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
markGroups = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.markGroups)
  (milestoneContext . _1 . _1 $ aroundFrom Around.markGroups)

assocLetterFinal :: AroundMilestone Void Void
  (([(Letter :* Case :* Final) :* [Mark]] :* Elision) :* SentenceBoundary)
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
assocLetterFinal = Around
  (milestoneContext . _1 . _1 . travList $ aroundTo around)
  (milestoneContext . _1 . _1 . travList $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to ((l, (c, f)), ms) = ((l, f), (c, ms))
  from ((l, f), (c, ms)) = ((l, (c, f)), ms)

final :: AroundMilestone [Around.InvalidFinals] Void
  (([(Letter :* Final) :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
  (([Letter :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
final = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.final)
  (milestoneContext . _1 . _1 $ aroundFrom Around.final)

capitalization :: AroundMilestone Around.InvalidUppercase Void
  (([Letter :* Case :* [Mark]] :* Elision) :* SentenceBoundary)
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
capitalization = Around
  (milestoneContext . _1 . _1 $ aroundTo Around.capitalization)
  (milestoneContext . _1 . _1 $ aroundFrom Around.capitalization)

markSplit :: AroundMilestone Around.InvalidMarkCombo Void
  ((([Letter :* [Mark]] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
markSplit = Around
  (milestoneContext . _1 . _1 . _1 . travList . _2 $ aroundTo Around.markSplit)
  (milestoneContext . _1 . _1 . _1 . travList . _2 $ aroundFrom Around.markSplit)

letterVowelConsonant :: AroundMilestone Void Void
  ((([Letter :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
letterVowelConsonant = Around
  (milestoneContext . _1 . _1 . _1 . travList . _1 $ aroundTo Around.letterVowelConsonant)
  (milestoneContext . _1 . _1 . _1 . travList . _1 $ aroundFrom Around.letterVowelConsonant)

distVowelConsonantMarks :: AroundMilestone Void Void
  ((([(Vowel :+ Consonant) :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark] :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
distVowelConsonantMarks = Around
  (milestoneContext . _1 . _1 . _1 . travList $ aroundTo Around.distLeftSumOverProd)
  (milestoneContext . _1 . _1 . _1 . travList $ aroundFrom Around.distLeftSumOverProd)

consonantMarks :: AroundMilestone Around.InvalidConsonantMarks Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ (Consonant :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
consonantMarks = Around
  (milestoneContext . _1 . _1 . _1 . travList . _Right $ aroundTo Around.consonantMarks)
  (milestoneContext . _1 . _1 . _1 . travList . _Right $ aroundFrom Around.consonantMarks)

groupVowelConsonants :: AroundMilestone Void Void
  ((([ (Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark)
    :+ ConsonantRho
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
groupVowelConsonants = Around
  (milestoneContext . _1 . _1 . _1 $ aroundTo Around.groupSums)
  (milestoneContext . _1 . _1 . _1 $ aroundFrom Around.groupSums)

vowelSyllabicMark :: AroundMilestone Void Void
  ((([ [Vowel :* Maybe Accent :* Maybe Breathing :* Maybe SyllabicMark]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe Accent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
vowelSyllabicMark = Around
  (milestoneContext . _1 . _1 . _1 . travList . _Left . travList $ aroundTo around)
  (milestoneContext . _1 . _1 . _1 . travList . _Left . travList $ aroundFrom around)
  where
  around = Around.makeIdAround to from
  to (x, (y, (z, q))) = (x, (q, (y, z)))
  from (x, (q, (y, z))) = (x, (y, (z, q)))

vocalicSyllable :: AroundMilestone Void Void
  ((([ [Vowel :* Maybe SyllabicMark :* Maybe Accent :* Maybe Breathing]
    :+ [ConsonantRho]
    ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
vocalicSyllable = Around
  (milestoneContext . _1 . _1 . _1 . travList . _Left $ aroundTo $ Around.vocalicSyllable (Nothing, Nothing))
  (milestoneContext . _1 . _1 . _1 . travList . _Left $ aroundFrom $ Around.vocalicSyllable (Nothing, Nothing))

swapConsonantVocalicSyllables :: AroundMilestone Void Void
  ((([ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] :+ [ConsonantRho] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
swapConsonantVocalicSyllables = Around
  (milestoneContext . _1 . _1 . _1 . travList $ aroundTo Around.swapSum)
  (milestoneContext . _1 . _1 . _1 . travList $ aroundFrom Around.swapSum)

ungroupConsonantVocalicSyllables :: AroundMilestone Void Void
  ((([ [ConsonantRho] :+ [VocalicSyllable :* Maybe Accent :* Maybe Breathing] ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
ungroupConsonantVocalicSyllables = Around
  (milestoneContext . _1 . _1 . _1 $ aroundTo Around.ungroupSums)
  (milestoneContext . _1 . _1 . _1 $ aroundFrom Around.ungroupSums)

groupLeftConsonantVocalicSyllables :: AroundMilestone Void Void
  ((([ ConsonantRho :+ (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ]
    :* Capitalization) :* Elision) :* SentenceBoundary)
  (((([ [ConsonantRho] :* (VocalicSyllable :* Maybe Accent :* Maybe Breathing) ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
groupLeftConsonantVocalicSyllables = Around
  (milestoneContext . _1 . _1 . _1 $ aroundTo Around.groupLeft)
  (milestoneContext . _1 . _1 . _1 $ aroundFrom Around.groupLeft)

breathing :: AroundMilestone [Around.InvalidBreathing ConsonantRho VocalicSyllable (Maybe Accent)] Void
  (((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent :* Maybe Breathing ] :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
breathing = Around
  (milestoneContext . _1 . _1 . _1 . _1 $ aroundTo Around.breathing)
  (milestoneContext . _1 . _1 . _1 . _1 $ aroundFrom Around.breathing)

reorderWordProps :: AroundMilestone Void Void
  ((((([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* MarkPreservation :* Crasis :* InitialAspiration) :* [ConsonantRho])
    :* Capitalization) :* Elision) :* SentenceBoundary)
  ([ [ConsonantRho] :* VocalicSyllable :* Maybe Accent ] :* [ConsonantRho]
    :* MarkPreservation :* Crasis :* InitialAspiration :* Capitalization :* Elision :* SentenceBoundary)
reorderWordProps = Around
  (milestoneContext $ aroundTo around)
  (milestoneContext $ aroundFrom around)
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
