module Prepare.Perseus.Paths where

import System.FilePath ((</>))

getPerseusPath :: FilePath -> String -> String -> String -> FilePath
getPerseusPath dataPath g t v = dataPath </> "xml-perseus-greek/data/" ++ g ++ "/" ++ t ++ "/" ++ g ++ "." ++ t ++ ".perseus-grc" ++ v ++ ".xml"

perseusShortList :: FilePath -> [FilePath]
perseusShortList dataPath = fmap (dataPath </>)
  [ "xml-perseus-greek/data/tlg0001/tlg001/tlg0001.tlg001.perseus-grc2.xml" -- ok
  , "xml-perseus-greek/data/tlg0003/tlg001/tlg0003.tlg001.perseus-grc2.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg002/tlg0014.tlg002.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg005/tlg0014.tlg005.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg011/tlg0014.tlg011.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg012/tlg0014.tlg012.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg014/tlg0014.tlg014.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg026/tlg0014.tlg026.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg051/tlg0014.tlg051.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg060/tlg0014.tlg060.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0014/tlg061/tlg0014.tlg061.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0032/tlg001/tlg0032.tlg001.perseus-grc2.xml"
  , "xml-perseus-greek/data/tlg0032/tlg002/tlg0032.tlg002.perseus-grc2.xml"
  , "xml-perseus-greek/data/tlg0032/tlg003/tlg0032.tlg003.perseus-grc2.xml"
  , "xml-perseus-greek/data/tlg0032/tlg004/tlg0032.tlg004.perseus-grc2.xml"
  , "xml-perseus-greek/data/tlg0032/tlg005/tlg0032.tlg005.perseus-grc2.xml" -- ok
  , "xml-perseus-greek/data/tlg0032/tlg008/tlg0032.tlg008.perseus-grc2.xml"
  , "xml-perseus-greek/data/tlg0032/tlg009/tlg0032.tlg009.perseus-grc2.xml" -- ok
  , "xml-perseus-greek/data/tlg0032/tlg010/tlg0032.tlg010.perseus-grc1.xml"
  , "xml-perseus-greek/data/tlg0032/tlg011/tlg0032.tlg011.perseus-grc1.xml"
  , "xml-perseus-greek/data/tlg0032/tlg012/tlg0032.tlg012.perseus-grc1.xml"
  , "xml-perseus-greek/data/tlg0032/tlg013/tlg0032.tlg013.perseus-grc1.xml" -- ok
  , "xml-perseus-greek/data/tlg0032/tlg014/tlg0032.tlg014.perseus-grc1.xml" -- ok
  ]
