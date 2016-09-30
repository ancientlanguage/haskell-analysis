module Prepare.Sblgnt.Unify where

import Data.Text (Text)
import Prepare.Language
import qualified Prepare.Sblgnt.Model as Sblgnt
import qualified Prepare.Source.Model as Source

unify :: Sblgnt.Sblgnt -> Source.Group
unify (Sblgnt.Sblgnt st sl bs) = Source.Group "Sblgnt" Greek "SBLGNT" lic
  (fmap (bookSource lic) bs)
  where
  lic = []

bookSource :: [Text] -> Sblgnt.Book -> Source.Source
bookSource lic (Sblgnt.Book bid btitle bp be) = Source.Source
  (shortIdToLong bid)
  btitle
  lic
  (concatMap flatParagraph bp)

flatParagraph :: Sblgnt.Paragraph -> [Source.Content]
flatParagraph (Sblgnt.Paragraph cs) = p : fmap content cs
  where p = Source.ContentMilestone Source.MilestoneParagraph

content :: Sblgnt.Content -> Source.Content
content (Sblgnt.ContentVerse v) = Source.ContentMilestone (verse v)
content (Sblgnt.ContentWord w) = Source.ContentWord (word w)

verse :: Sblgnt.Verse -> Source.Milestone
verse (Sblgnt.Verse _ cn vn _) = Source.MilestoneVerse (Source.Verse cn vn)

word :: Sblgnt.Word -> Source.Word
word (Sblgnt.Word p t s) = Source.Word p t s

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
