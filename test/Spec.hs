import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Prepare
import Prepare.Perseus.Paths
import Prepare.Perseus.TeiEpidocParser (tei)
import Prepare.Sblgnt.Parser (sblgnt)
import Prepare.Tanach.IndexParser (index)
import qualified Prepare.Tanach.IndexModel as Index
import Prepare.Tanach.HeaderParser (header)
import Prepare.Tanach.TanachParser (tanach)

parseTanachHeader :: Test
parseTanachHeader = undefined

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
  , testGroup "Tanach Header" [ parseTanachHeader ]
  ]
