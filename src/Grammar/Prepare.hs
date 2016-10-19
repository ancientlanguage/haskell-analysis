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

withMilestone
  :: (a -> Validation [c] b)
  -> Milestone :* a
  -> Validation [Milestone :* a :* c] (Milestone :* b)
withMilestone f (m , a) =
  case f a of
    Failure c -> Failure (fmap (\x -> (m , (a , x))) c)
    Success b -> Success (m , b)

allWordsPath
  :: (a -> Validation [c] b)
  -> [SourceId :* [Milestone :* a]]
  -> Validation [Milestone :* a :* c] [SourceId :* [Milestone :* b]]
allWordsPath f = traverse . _2 . traverse $ withMilestone f

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
