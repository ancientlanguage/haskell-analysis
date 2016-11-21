module Grammar.Common.Pretty where

import Data.Text (Text)
import qualified Data.Text as Text
import Grammar.Common.Types

textShow :: Show a => a -> Text
textShow = Text.pack . show

prettyMilestone :: Maybe Division :* Maybe Paragraph -> Text
prettyMilestone (Nothing, _) = ""
prettyMilestone (Just (Division b c v s l), _)
  = Text.intercalate "."
  . filter (\x -> not . Text.null $ x)
  $ [ms b, ms c, ms v, ms s, ms l]
  where
  ms Nothing = ""
  ms (Just x) = textShow x

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

prettyMilestoneCtx :: Milestone :* Text :* [Text] :* [Text] -> (Text, Text, Text, Text)
prettyMilestoneCtx (m, (w, (ls, rs))) =
  ( prettyMilestone m
  , Text.intercalate " " ls
  , w
  , Text.intercalate " " rs
  )
