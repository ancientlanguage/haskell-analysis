module Prepare.Perseus.Paths where

getPerseusPath :: String -> String -> String -> FilePath
getPerseusPath g t v = "./data/xml-perseus-greek/data/" ++ g ++ "/" ++ t ++ "/" ++ g ++ "." ++ t ++ ".perseus-grc" ++ v ++ ".xml"

perseusShortList :: [FilePath]
perseusShortList =
  [ getPerseusPath "tlg0001" "tlg001" "2"
  , getPerseusPath "tlg0014" "tlg005" "1"
  , getPerseusPath "tlg0014" "tlg011" "1"
  , getPerseusPath "tlg0014" "tlg012" "1"
  , getPerseusPath "tlg0014" "tlg014" "1"
  , getPerseusPath "tlg0014" "tlg026" "1"
  , getPerseusPath "tlg0014" "tlg051" "1"
  , getPerseusPath "tlg0014" "tlg060" "1"
  , getPerseusPath "tlg0014" "tlg061" "1"

  , getPerseusPath "tlg0032" "tlg001" "2"
  , getPerseusPath "tlg0032" "tlg002" "2"
  , getPerseusPath "tlg0032" "tlg003" "2"
  , getPerseusPath "tlg0032" "tlg004" "2"
  , getPerseusPath "tlg0032" "tlg005" "2"
  --    , getPerseusPath "tlg0032" "tlg006" "1"
  --    , getPerseusPath "tlg0032" "tlg007" "1"
  , getPerseusPath "tlg0032" "tlg008" "1"
  , getPerseusPath "tlg0032" "tlg009" "1"
  , getPerseusPath "tlg0032" "tlg010" "1"
  , getPerseusPath "tlg0032" "tlg011" "1"
  , getPerseusPath "tlg0032" "tlg012" "1"
  , getPerseusPath "tlg0032" "tlg013" "1"
  , getPerseusPath "tlg0032" "tlg014" "1"
  ]
