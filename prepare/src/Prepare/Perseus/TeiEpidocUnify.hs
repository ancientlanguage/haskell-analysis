module Prepare.Perseus.TeiEpidocUnify where

import Prelude hiding (getContents)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Prepare.Perseus.TeiEpidocHeaderModel as Header
import Prepare.Perseus.TeiEpidocModel
import qualified Primary as Primary

dropSuffixIgnoreCase :: [Text] -> [Text] -> [Text]
dropSuffixIgnoreCase ss xs =
  if List.isSuffixOf ss (fmap Text.toLower xs)
  then List.take (length xs - length ss) xs
  else xs

makeId :: Text -> Text -> Text
makeId author title = Text.intercalate "_" [hack author, hack title]
  where
  hack :: Text -> Text
  hack
    = Text.dropWhileEnd (\x -> Char.isPunctuation x || x == '_')
    . Text.filter (\x -> Char.isLetter x || Char.isNumber x || x == '_')
    . Text.intercalate "_"
    . dropSuffixIgnoreCase ["from", "minor", "works"]
    . List.filter (\x -> Text.toLower x /= "(greek)")
    . dropSuffixIgnoreCase ["machine", "readable", "text"]
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

getMilestoneContents :: Milestone -> [Primary.Milestone]
getMilestoneContents (MilestoneParagraph _) = [Primary.MilestoneParagraph]
getMilestoneContents (MilestoneCard n) = [Primary.MilestoneCard n]

isGreekChar :: Char -> Bool
isGreekChar x
  = x /= '\x037e' -- Greek question mark
  && ((x >= '\x0370' && x <= '\x03ff')
    || (x >= '\x1f00' && x <= '\x1fff'))

buildPrimaryWord :: Text -> Primary.Word
buildPrimaryWord t = Primary.Word pre (Text.map unifyApostrophe core) suff
  where
  pre = Text.takeWhile (not . isCore) $ t
  core = Text.takeWhile isCore . Text.drop (Text.length pre) $ t
  suff = Text.drop (Text.length pre + Text.length core) $ t
  isCore x = isGreekChar x || isApostrophe x
  isApostrophe x = x == '\x02BC' || x == '\x2019' || x == '\x0027' || x == '\x1FBD'
  unifyApostrophe x | isApostrophe x = '\x2019'
  unifyApostrophe x = x

removeApparatusMarks :: Primary.Word -> Primary.Word
removeApparatusMarks (Primary.Word pre core suff) = Primary.Word (remove pre) core (remove suff)
  where
  remove = Text.filter (not . isApparatusChar) . Text.replace "[;" "" . Text.replace "];" ""
  isApparatusChar x
    = x == '['
    || x == ']'
    || x == '†'
    || x == '<'
    || x == '>'

removeContentApparatusMarks :: Primary.Content -> Primary.Content
removeContentApparatusMarks (Primary.ContentWord w) = Primary.ContentWord $ removeApparatusMarks w
removeContentApparatusMarks x = x

extractWords :: Text -> [Primary.Content]
extractWords = fmap (Primary.ContentWord . buildPrimaryWord) . Text.words

getQuoteLineContents :: QuoteLine -> [Either Primary.Milestone Text]
getQuoteLineContents (QuoteLine _ t _) = [Right $ Text.concat [" ", t, " "]]

getQuoteContents :: Quote -> [Either Primary.Milestone Text]
getQuoteContents (Quote _ ls) = Right " " : concatMap getQuoteLineContents ls ++ [Right " "]

getCitContents :: Cit -> [Either Primary.Milestone Text]
getCitContents (Cit q _) = getQuoteContents q

getSicContents :: Sic -> [Text]
getSicContents = Maybe.maybeToList . sicContent

getApparatusAddContents :: ApparatusAdd -> [Text]
getApparatusAddContents (ApparatusAdd t) = [t]

getApparatusDelContents :: ApparatusDel -> [Text]
getApparatusDelContents (ApparatusDel t) = [t]

getApparatusCorrContents :: ApparatusCorr -> [Text]
getApparatusCorrContents (ApparatusCorr t) = [t]

getTermContents :: Term -> [Text]
getTermContents (Term t) = [t]

getSpeakerContents :: Speaker -> [Text]
getSpeakerContents (Speaker _ p) = [p]

unifyContent :: Content -> [Either Primary.Milestone Text]
unifyContent (ContentMilestone m) = fmap Left $ getMilestoneContents m
unifyContent (ContentText t) = [Right t]
unifyContent (ContentAdd t) = fmap Right $ getApparatusAddContents t
unifyContent (ContentCorr t) = fmap Right $ getApparatusCorrContents t
unifyContent (ContentDel t) = fmap Right $ getApparatusDelContents t
unifyContent (ContentTerm t) = fmap Right $ getTermContents t
unifyContent (ContentGap _) = []
unifyContent (ContentQuote q) = getQuoteContents q
unifyContent (ContentBibl _) = []
unifyContent (ContentCit c) = getCitContents c
unifyContent (ContentSpeaker s) = fmap Right $ getSpeakerContents s
unifyContent (ContentSic s) = fmap Right $ getSicContents s

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

processContents :: [Either Primary.Milestone Text] -> [Primary.Content]
processContents = process . groupRight
  where
  process (x, ys) = textContent x ++ concatMap (\(m, xs) -> Primary.ContentMilestone m : textContent xs) ys
  textContent
    = fmap removeContentApparatusMarks
    . concatMap splitWordInSuffix
    . mergeSuffixes
    . extractWords
    . Text.concat
  groupRight :: [Either a b] -> ([b], [(a, [b])])
  groupRight = foldr go ([], [])
  go (Left x) (ms, ys) = ([], (x, ms) : ys)
  go (Right m) (ms, ys) = (m : ms, ys)

getSectionContents :: Primary.Division -> Section -> [Primary.Content]
getSectionContents d (Section sn cs)
  = Primary.ContentMilestone (Primary.MilestoneDivision (d { Primary.divisionSection = Just sn }))
  : processContents (concatMap unifyContent cs)

getChapterContents :: Primary.Division -> Chapter -> [Primary.Content]
getChapterContents d (Chapter cn ss) = concatMap (getSectionContents $ d { Primary.divisionChapter = Just cn}) ss

getBookContents :: Primary.Division -> Book -> [Primary.Content]
getBookContents d (Book bn _ cs) = concatMap (getChapterContents $ d { Primary.divisionBook = Just bn }) cs

getLineContentContents :: LineContent -> [Either Primary.Milestone Text]
getLineContentContents (LineContentMilestone m) = fmap Left $ getMilestoneContents m
getLineContentContents (LineContentText t) = [Right t]
getLineContentContents (LineContentDel x) = fmap Right $ getApparatusDelContents x

getLineContents :: Primary.Division -> Line -> [Primary.Content]
getLineContents d (Line n _ cs)
  = tryAdd n $ processContents (concatMap getLineContentContents cs)
  where
  tryAdd Nothing xs = xs
  tryAdd v@(Just _) xs = Primary.ContentMilestone (Primary.MilestoneDivision (d { Primary.divisionLine = v })) : xs

getBookLineContentContents :: Primary.Division -> BookLineContent -> [Primary.Content]
getBookLineContentContents _ (BookLineContentMilestone m) = fmap Primary.ContentMilestone $ getMilestoneContents m
getBookLineContentContents d (BookLineContentLine ln) = getLineContents d ln

getBookLinesContents :: Primary.Division -> BookLines -> [Primary.Content]
getBookLinesContents d (BookLines bn cs) = concatMap (getBookLineContentContents $ d { Primary.divisionBook = Just bn }) cs

emptyDivision :: Primary.Division
emptyDivision = Primary.Division Nothing Nothing Nothing Nothing Nothing

getDivisionContents :: Division -> [Primary.Content]
getDivisionContents (DivisionBooks xs) = concatMap (getBookContents emptyDivision) xs
getDivisionContents (DivisionChapters xs) = concatMap (getChapterContents emptyDivision) xs
getDivisionContents (DivisionSections xs) = concatMap (getSectionContents emptyDivision) xs
getDivisionContents (DivisionBookLines xs) = concatMap (getBookLinesContents emptyDivision) xs

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
