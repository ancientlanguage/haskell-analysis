module Grammar.Common.Prepare where

import Prelude hiding (Word)
import Control.Lens hiding ((:>))
import Data.Either.Validation
import Data.Text (Text)
import qualified Primary
import Grammar.Common.Types

toLocalDivision :: Primary.Division -> Division
toLocalDivision (Primary.Division b c v s) = Division b c v s

emptyMilestone :: Milestone
emptyMilestone = (Nothing, Nothing)

nextParagraph :: Maybe Paragraph -> Maybe Paragraph
nextParagraph Nothing = Just (Paragraph 0)
nextParagraph (Just (Paragraph p)) = Just (Paragraph (p + 1))

withItemContext
  :: (a -> Validation [e] b)
  -> ctx :* a
  -> Validation [ctx :* a :* e] (ctx :* b)
withItemContext f (ctx , a) =
  case f a of
    Failure es -> Failure (fmap (\x -> (ctx , (a , x))) es)
    Success b -> Success (ctx , b)

traverseWithItemContext
  :: forall a b c ctx
  . (a -> Validation [c] b)
  -> [ctx :* a]
  -> Validation
    [ctx :* a :* c]
    [ctx :* b]
traverseWithItemContext f = traverse (withItemContext f)

prepareContents :: [Primary.Content] -> [Milestone :* Primary.Word]
prepareContents = go emptyMilestone
  where
  go :: Milestone -> [Primary.Content] -> [Milestone :* Primary.Word]
  go _ [] = []
  go m (Primary.ContentMilestone (Primary.MilestoneDivision x) : xs) = go (over _1 (const (Just (toLocalDivision x))) m) xs
  go m (Primary.ContentMilestone Primary.MilestoneParagraph : xs) = go (over _2 nextParagraph m) xs
  go m (Primary.ContentWord w : xs) = (m , w) : go m xs

prepareSource :: Text -> Primary.Source -> SourceId :* [Milestone :* Primary.Word]
prepareSource gid s = (SourceId gid (Primary.sourceId s) , prepareContents (Primary.sourceContents s))

prepareGroup :: Primary.Group -> [SourceId :* [Milestone :* Primary.Word]]
prepareGroup g = prepareSource (Primary.groupId g) <$> Primary.groupSources g

prepareGroups :: [Primary.Group] -> [SourceId :* [Milestone :* Primary.Word]]
prepareGroups = concatMap prepareGroup
