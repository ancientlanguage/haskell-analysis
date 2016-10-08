module Prepare.Sblgnt.Unify where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Prepare.Language
import qualified Prepare.Sblgnt.Model as Sblgnt
import qualified Prepare.Source.Model as Source

unify :: Sblgnt.Sblgnt -> Source.Group
unify (Sblgnt.Sblgnt st sl bs) =
  Source.Group "Sblgnt" Greek (getTitle desc) desc contents
  where
  groupId = "Sblgnt"
  getTitle [] = groupId
  getTitle (x : _) = x
  desc = descLines st sl
  contents = fmap (bookSource desc) bs

descLines :: [Sblgnt.HeadParagraph] -> [Sblgnt.HeadParagraph] -> [Text]
descLines st sl = headParagraphLines st ++ headParagraphLines sl

headParagraphLines :: [Sblgnt.HeadParagraph] -> [Text]
headParagraphLines = fmap headParagraphText

headParagraphText :: Sblgnt.HeadParagraph -> Text
headParagraphText = Text.concat . fmap headContentText . Sblgnt.headParagraphContents  

headContentText :: Sblgnt.HeadContent -> Text
headContentText (Sblgnt.HeadContentText t) = t
headContentText (Sblgnt.HeadContentLink (Sblgnt.Link h _)) = h 

bookSource :: [Text] -> Sblgnt.Book -> Source.Source
bookSource lic (Sblgnt.Book bid btitle bp _) =
  Source.Source
    (shortIdToLong bid)
    btitle
    lic
    (concatMap flatParagraph bp)

flatParagraph :: Sblgnt.Paragraph -> [Source.Content]
flatParagraph (Sblgnt.Paragraph cs) = if null cs then [] else p : fmap content cs
  where
  p = Source.ContentMilestone Source.MilestoneParagraph

content :: Sblgnt.Content -> Source.Content
content (Sblgnt.ContentVerse v) = Source.ContentMilestone (verse v)
content (Sblgnt.ContentWord w) = Source.ContentWord (word w)

verse :: Sblgnt.Verse -> Source.Milestone
verse (Sblgnt.Verse _ cn vn _) = Source.MilestoneVerse (Source.Verse cn vn)

word :: Sblgnt.Word -> Source.Word
word (Sblgnt.Word p t s) = Source.Word p' t s'
  where
  ignore cs = Text.filter (not . (\x -> Char.isSpace x || flip Set.member cs x))
  p' = ignore ignoredPrefixChars (Maybe.maybe "" id p)
  s' = ignore ignoredSuffixChars s

ignoredPrefixChars :: Set Char
ignoredPrefixChars = Set.fromList
  [ '\x2E00' -- RIGHT ANGLE SUBSTITUTION MARKER
  , '\x2E01' -- RIGHT ANGLE DOTTED SUBSTITUTION MARKER
  , '\x2E02' -- LEFT SUBSTITUTION BRACKET
  , '\x2E04' -- LEFT DOTTED SUBSTITUTION BRACKET
  , '['
  , '1'
  , '2'
  ]

ignoredSuffixChars :: Set Char
ignoredSuffixChars = Set.fromList
  [ '\x2E02' -- LEFT SUBSTITUTION BRACKET
  , '\x2E03' -- RIGHT SUBSTITUTION BRACKET
  , '\x2E05' -- RIGHT DOTTED SUBSTITUTION BRACKET
  , ']'
  ]

shortIdToLong :: Text -> Text
shortIdToLong "Mt" = "Matthew"
shortIdToLong "Mk" = "Mark"
shortIdToLong "Lu" = "Luke"
shortIdToLong "Jn" = "John"
shortIdToLong "Ac" = "Acts"
shortIdToLong "Ro" = "Romans"
shortIdToLong "1Co" = "1Corinthians"
shortIdToLong "2Co" = "2Corinthians"
shortIdToLong "Gal" = "Galatians"
shortIdToLong "Eph" = "Ephesians"
shortIdToLong "Php" = "Philippians"
shortIdToLong "Col" = "Colossians"
shortIdToLong "1Th" = "1Thessalonians"
shortIdToLong "2Th" = "2Thessalonians"
shortIdToLong "1Tim" = "1Timothy"
shortIdToLong "2Tim" = "2Timothy"
shortIdToLong "Tit" = "Titus"
shortIdToLong "Phm" = "Philemon"
shortIdToLong "Heb" = "Hebrews"
shortIdToLong "Jam" = "James"
shortIdToLong "1Pe" = "1Peter"
shortIdToLong "2Pe" = "2Peter"
shortIdToLong "1Jn" = "1John"
shortIdToLong "2Jn" = "2John"
shortIdToLong "3Jn" = "3John"
shortIdToLong "Jud" = "Jude"
shortIdToLong "Re" = "Revelation"
shortIdToLong t = t
