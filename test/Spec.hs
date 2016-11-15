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

parsePerseusTeiEpidoc :: Test
parsePerseusTeiEpidoc = testCase "parse perseus tei epidoc" $ do
  let xmlPath = "./data/xml-perseus-greek/data/tlg0032/tlg001/tlg0032.tlg001.perseus-grc2.xml"
  loadParse xmlPath tei logBook >>= \case
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Perseus" [ parsePerseusTeiEpidoc ]
  , testGroup "SBLGNT" [ parseSblgnt ]
  ]
