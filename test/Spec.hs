import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Prepare

parseSblgnt :: Test
parseSblgnt = testCase "parse sblgnt" $ do
  let xmlPath = "./data/xml-sblgnt/sblgnt.xml"
  loadParse xmlPath sblgnt logBook >>= \case  
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

parsePerseusTeiEpidoc :: String -> String -> String -> Test
parsePerseusTeiEpidoc g t v = testCase ("parse perseus tei epidoc: " ++ g ++ "." ++ t ++ "-" ++ v) $ do
  let xmlPath = "./data/xml-perseus-greek/data/" ++ g ++ "/" ++ t ++ "/" ++ g ++ "." ++ t ++ ".perseus-grc" ++ v ++ ".xml"
  loadParse xmlPath tei logBook >>= \case
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Perseus"
    [ parsePerseusTeiEpidoc "tlg0032" "tlg001" "2"
    , parsePerseusTeiEpidoc "tlg0032" "tlg002" "2"
    , parsePerseusTeiEpidoc "tlg0032" "tlg003" "2"
    , parsePerseusTeiEpidoc "tlg0032" "tlg004" "2"
    , parsePerseusTeiEpidoc "tlg0032" "tlg005" "2"
--    , parsePerseusTeiEpidoc "tlg0032" "tlg006" "1"
--    , parsePerseusTeiEpidoc "tlg0032" "tlg007" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg008" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg009" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg010" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg011" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg012" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg013" "1"
    , parsePerseusTeiEpidoc "tlg0032" "tlg014" "1"
    ]
  , testGroup "SBLGNT" [ parseSblgnt ]
  ]
