module Prepare.Source.DecomposeWords where

import Prelude hiding (Word)
import Prepare.Decompose
import Primary

decomposeWord :: Word -> Word
decomposeWord (Word p t s) = Word (decompose p) (decompose t) (decompose s)

decomposeContent :: Content -> Content
decomposeContent m@(ContentMilestone _) = m
decomposeContent (ContentWord w) = ContentWord (decomposeWord w)

decomposeSource :: Source -> Source
decomposeSource x = x { sourceContents = fmap decomposeContent (sourceContents x) }

decomposeGroup :: Group -> Group
decomposeGroup x = x { groupSources = fmap decomposeSource (groupSources x) }

decomposeGroups :: [Group] -> [Group]
decomposeGroups = fmap decomposeGroup
