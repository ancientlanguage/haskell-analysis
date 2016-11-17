module Prepare.Perseus.TeiEpidocUnify where

import Prelude hiding (getContents)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Prepare.Perseus.TeiEpidocHeaderModel as Header
import Prepare.Perseus.TeiEpidocModel
import qualified Primary as Primary

makeId :: Text -> Text -> Text
makeId author title = Text.intercalate "_" [hack author, hack title]
  where
  hack :: Text -> Text
  hack
    = Text.dropWhileEnd (\x -> Char.isPunctuation x || x == '_')
    . Text.filter (\x -> Char.isLetter x || Char.isNumber x || x == '_')
    . Text.intercalate "_"
    . List.filter (\x -> Text.toLower x /= "(greek)")
    . (\xs ->
        if List.isSuffixOf ["machine", "readable", "text"] (fmap Text.toLower xs)
        then List.take (length xs - 3) xs
        else xs)
    . Text.words
    . Text.replace "-" " "
    . Text.replace "." " "

getSourceMetadata :: Header.TeiHeader -> Primary.Source
getSourceMetadata h = Primary.Source sid t (Just a) lic []
  where
  sid = makeId a t
  titleStmt = Header.fileDescTitleStmt . Header.teiHeaderFileDesc $ h
  t = Header.titleStmtTitle titleStmt
  a = Header.titleStmtAuthor titleStmt
  lic = []

getMilestoneContents :: Milestone -> [Primary.Content]
getMilestoneContents (Milestone u _) | u == "para" = pure $ Primary.ContentMilestone Primary.MilestoneParagraph
getMilestoneContents _ = []

isGreekChar :: Char -> Bool
isGreekChar x 
  = x /= '\x037e' -- Greek question mark
  && ((x >= '\x0370' && x <= '\x03ff')
    || (x >= '\x1f00' && x <= '\x1fff'))

buildPrimaryWord :: Text -> Primary.Word
buildPrimaryWord t
  = Primary.Word
    (removePrefixApparatusMarks pre)
    (Text.map unifyApostrophe core)
    (removeSuffixApparatusMarks suff)
  where
  pre = Text.takeWhile (not . isGreekChar) $ t
  core = Text.takeWhile isCore . Text.drop (Text.length pre) $ t
  suff = Text.drop (Text.length pre + Text.length core) $ t
  isCore x = isGreekChar x || isApostrophe x
  isApostrophe x = x == '\x02BC' || x == '\x2019' || x == '\x0027'
  unifyApostrophe x | isApostrophe x = '\x2019'
  unifyApostrophe x = x
  removePrefixApparatusMarks = Text.filter (not . isPrefixApparatusChar) . Text.replace "[;" ""
  isPrefixApparatusChar x
    = x == '<'
    || x == '['
    || x == '†'
  removeSuffixApparatusMarks = Text.filter (not . isSuffixApparatusChar) . Text.replace "];" ""
  isSuffixApparatusChar x
    = x == '['
    || x == ']'
    || x == '†'
    || x == ']'
    || x == '>'

extractWords :: Text -> [Primary.Content]
extractWords = fmap (Primary.ContentWord . buildPrimaryWord) . Text.words

getLineContents :: Line -> [Primary.Content]
getLineContents (Line _ t) = extractWords t

getQuoteContents :: Quote -> [Primary.Content]
getQuoteContents (Quote _ ls) = concatMap getLineContents ls

getCitContents :: Cit -> [Primary.Content]
getCitContents (Cit q _) = getQuoteContents q

unifyContent :: Content -> [Primary.Content]
unifyContent (ContentMilestone m) = getMilestoneContents m
unifyContent (ContentText t) = extractWords t
unifyContent (ContentAdd t) = extractWords t
unifyContent (ContentCorr t) = extractWords t
unifyContent (ContentDel t) = extractWords t
unifyContent (ContentTerm t) = extractWords t
unifyContent (ContentGap _) = []
unifyContent (ContentQuote q) = getQuoteContents q
unifyContent (ContentBibl _) = []
unifyContent (ContentCit c) = getCitContents c

mergeSuffixes :: [Primary.Content] -> [Primary.Content]
mergeSuffixes = foldr go []
  where
  go (Primary.ContentWord (Primary.Word p t s)) (Primary.ContentWord (Primary.Word p' t' s') : cs)
    | Text.null t' = Primary.ContentWord (Primary.Word p t (Text.concat [s, p', s'])) : cs
  go c cs = c : cs

splitWordInSuffix :: Primary.Content -> [Primary.Content]
splitWordInSuffix (Primary.ContentWord (Primary.Word p t s)) | Text.any isGreekChar s
  = let (Primary.Word p' t' s') = buildPrimaryWord s
  in fmap Primary.ContentWord [Primary.Word p t p', Primary.Word "" t' s']
splitWordInSuffix c = [c]

getSectionContents :: Primary.Division -> Section -> [Primary.Content]
getSectionContents d (Section sn cs)
  = Primary.ContentMilestone (Primary.MilestoneDivision (d { Primary.divisionSection = Just sn }))
  : (concatMap splitWordInSuffix . mergeSuffixes . concatMap unifyContent $ cs)

getChapterContents :: Primary.Division -> Chapter -> [Primary.Content]
getChapterContents d (Chapter cn ss) = concatMap (getSectionContents $ d { Primary.divisionChapter = Just cn}) ss

getBookContents :: Primary.Division -> Book -> [Primary.Content]
getBookContents d (Book bn _ cs) = concatMap (getChapterContents $ d { Primary.divisionBook = Just bn }) cs

emptyDivision :: Primary.Division
emptyDivision = Primary.Division Nothing Nothing Nothing Nothing

getDivisionContents :: Division -> [Primary.Content]
getDivisionContents (DivisionBooks xs) = concatMap (getBookContents emptyDivision) xs
getDivisionContents (DivisionChapters xs) = concatMap (getChapterContents emptyDivision) xs
getDivisionContents (DivisionSections xs) = concatMap (getSectionContents emptyDivision) xs

getEditionContents :: Edition -> [Primary.Content]
getEditionContents (Edition _ _ d) = getDivisionContents d

getBodyContents :: Body -> [Primary.Content]
getBodyContents (BodyEdition e) = getEditionContents e
getBodyContents (BodyDivision d) = getDivisionContents d

getTextContents :: TeiText -> [Primary.Content]
getTextContents = getBodyContents . teiTextBody

unify :: Tei -> Primary.Source
unify (Tei h t) = meta { Primary.sourceContents = getTextContents t }
  where
  meta = getSourceMetadata h

perseusGroup :: Primary.Group
perseusGroup = Primary.Group "Perseus" Primary.Greek "Perseus" ["Perseus texts"] []
