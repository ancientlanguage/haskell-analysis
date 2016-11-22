import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Prepare
import Prepare.Perseus.Paths
import Prepare.Perseus.TeiEpidocParser (tei)
import Prepare.Sblgnt.Parser (sblgnt)
import Prepare.Tanach.IndexParser (index)
import qualified Prepare.Tanach.Paths as Paths
import Prepare.Tanach.HeaderParser (header)
import Prepare.Tanach.TanachParser (tanach)

testParse n f = testCase n $ do
  f >>= \case
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

parseTanachHeader :: Test
parseTanachHeader = testParse "tanach header" $ loadParse Paths.headerFilePath header emptyLog

parseSblgnt :: Test
parseSblgnt = testParse "parse sblgnt" $ loadParse xmlPath sblgnt logBook
  where
  xmlPath = "./data/xml-sblgnt/sblgnt.xml"

parsePerseusTeiEpidoc :: String -> Test
parsePerseusTeiEpidoc p = testParse ("parse perseus tei epidoc: " ++ p) $ loadParse p tei logBook

main :: IO ()
main = defaultMain
  [ testGroup "Perseus" (fmap parsePerseusTeiEpidoc perseusShortList)
  , testGroup "SBLGNT" [ parseSblgnt ]
  , testGroup "Tanach Header" [ parseTanachHeader ]
  ]
