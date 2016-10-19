module Grammar.Prepare where

import Prelude hiding (Word)
import Control.Lens hiding ((:>))
import Data.Either.Validation
import Data.Text (Text)
import qualified Primary
import Grammar.CommonTypes

type Milestone = Maybe Primary.Verse :* Maybe Paragraph

emptyMilestone :: Milestone
emptyMilestone = (Nothing, Nothing)

nextParagraph :: Maybe Paragraph -> Maybe Paragraph
nextParagraph Nothing = Just (Paragraph 0)
nextParagraph (Just (Paragraph p)) = Just (Paragraph (p + 1))

type Milestoned x = Milestone :* x
type Source x = SourceId :* x
type SourceWords x = Source [Milestoned x]
type AllWords x = [SourceWords x]

withItemContext
  :: (a -> Validation [e] b)
  -> ctx :* a
  -> Validation [ctx :* a :* e] (ctx :* b)
withItemContext f (ctx , a) =
  case f a of
    Failure es -> Failure (fmap (\x -> (ctx , (a , x))) es)
    Success b -> Success (ctx , b)

withContext
  :: (a -> Validation [e] b)
  -> ctx :* a
  -> Validation [ctx :* e] (ctx :* b)
withContext f (ctx , a) =
  case f a of
    Failure es -> Failure (fmap (\x -> (ctx , x)) es)
    Success b -> Success (ctx , b)

allWordsPath
  :: forall a b c. (a -> Validation [c] b)
  -> [SourceId :* [Milestone :* a]]
  -> Validation
    [SourceId :* Milestone :* a :* c]
    [SourceId :* [Milestone :* b]]
allWordsPath f = traverse (withContext go)
  where
  go
    :: [Milestone :* a]
    -> Validation
      [Milestone :* a :* c]
      [Milestone :* b]
  go = traverse (withItemContext f)

allWordsPathId
  :: (a -> b)
  -> [SourceId :* [Milestone :* a]]
  -> [SourceId :* [Milestone :* b]]
allWordsPathId = over (traverse . _2 . traverse . _2)

prepareContents :: [Primary.Content] -> [Milestoned Primary.Word]
prepareContents = go emptyMilestone
  where
  go :: Milestone -> [Primary.Content] -> [Milestoned Primary.Word]
  go _ [] = []
  go m (Primary.ContentMilestone (Primary.MilestoneVerse x) : xs) = go (over _1 (const (Just x)) m) xs
  go m (Primary.ContentMilestone Primary.MilestoneParagraph : xs) = go (over _2 nextParagraph m) xs 
  go m (Primary.ContentWord w : xs) = (m , w) : go m xs

prepareSource :: Text -> Primary.Source -> SourceWords Primary.Word
prepareSource gid s = (SourceId gid (Primary.sourceId s) , prepareContents (Primary.sourceContents s))

prepareGroup :: Primary.Group -> AllWords Primary.Word
prepareGroup g = prepareSource (Primary.groupId g) <$> Primary.groupSources g

prepareGroups :: [Primary.Group] -> AllWords Primary.Word
prepareGroups = concatMap prepareGroup
