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

-- tlg 01 02 03 04 05 06 07 08 09 10 11 12 13 14
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
    ]
  , testGroup "SBLGNT" [ parseSblgnt ]
  ]
