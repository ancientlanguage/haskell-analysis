module Grammar.Prepare where

import Prelude hiding (Word)
import Control.Lens hiding ((:>))
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
type SourceWords x = Source (Fwd (Milestoned x))
type AllWords x = Fwd (SourceWords x)

withMilestone
  :: (a -> Fwd c :+ b)
  -> Milestone :* a
  -> Fwd (Milestone :* a :* c) :+ Milestone :* b
withMilestone f (m , a) =
  case f a of
    Left c -> Left (fmap (\x -> (m , (a , x))) c)
    Right b -> Right (m , b)

allWordsPath
  :: (a -> Fwd c :+ b)
  -> Fwd (SourceId :* (Fwd (Milestone :* a)))
  -> Fwd (Milestone :* a :* c) :+ Fwd (SourceId :* (Fwd (Milestone :* b)))
allWordsPath f = traverse . _2 . traverse $ withMilestone f

prepareContents :: [Primary.Content] -> Fwd (Milestoned Primary.Word)
prepareContents = go emptyMilestone
  where
  go :: Milestone -> [Primary.Content] -> Fwd (Milestoned Primary.Word)
  go _ [] = F0
  go m (Primary.ContentMilestone (Primary.MilestoneVerse x) : xs) = go (over _1 (const (Just x)) m) xs
  go m (Primary.ContentMilestone Primary.MilestoneParagraph : xs) = go (over _2 nextParagraph m) xs 
  go m (Primary.ContentWord w : xs) = (m , w) :> go m xs

prepareSource :: Text -> Primary.Source -> SourceWords Primary.Word
prepareSource gid s = (SourceId gid (Primary.sourceId s) , prepareContents (Primary.sourceContents s))

prepareGroup :: Primary.Group -> AllWords Primary.Word
prepareGroup g = fmap (prepareSource (Primary.groupId g)) . listToFwd $ Primary.groupSources g
