module Grammar.Pretty where

import Data.Text (Text)
import qualified Data.Text as Text
import Grammar.CommonTypes
import Grammar.Prepare

textShow :: Show a => a -> Text
textShow = Text.pack . show

prettyMilestone :: Maybe Verse :* Maybe Paragraph -> Text
prettyMilestone (Nothing, _) = ""
prettyMilestone (Just (Verse c v), _) = Text.concat [textShow c, ":", textShow v]

prettySource :: Show a => SourceId :* Milestone :* a -> Text
prettySource (SourceId g s, (m, x)) = Text.intercalate " " $
  [ g
  , s
  , prettyMilestone m
  , "--"
  , textShow $ x
  ]

prettyMilestoned :: Show a => Milestone :* a -> Text
prettyMilestoned (m, x) = Text.intercalate " " $
  [ prettyMilestone m
  , "--"
  , textShow $ x
  ]

prettyMilestonedString :: Milestone :* String -> Text
prettyMilestonedString (m, x) = Text.intercalate " " $
  [ prettyMilestone m
  , "--"
  , Text.pack x
  ]

prettyMilestoneCtxString :: (Milestone :* [String] :* [String]) :* String -> (Text, Text, Text, Text)
prettyMilestoneCtxString ((m, (ls, rs)), x) =
  ( prettyMilestone m
  , Text.intercalate " " (fmap Text.pack ls)
  , Text.pack x
  , Text.intercalate " " (fmap Text.pack rs)
  )
