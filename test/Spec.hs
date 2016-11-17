import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Prepare
import Prepare.Perseus.Paths

parseSblgnt :: Test
parseSblgnt = testCase "parse sblgnt" $ do
  let xmlPath = "./data/xml-sblgnt/sblgnt.xml"
  loadParse xmlPath sblgnt logBook >>= \case  
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

parsePerseusTeiEpidoc :: String -> Test
parsePerseusTeiEpidoc p = testCase ("parse perseus tei epidoc: " ++ p) $ do
  loadParse p tei logBook >>= \case
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "Perseus" (fmap parsePerseusTeiEpidoc perseusShortList)
  , testGroup "SBLGNT" [ parseSblgnt ]
  ]
