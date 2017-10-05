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

testParse :: TestName -> IO (Either [Char] t) -> Test
testParse n f = testCase n $ do
  f >>= \case
    Left e -> assertFailure $ "loadParse failure:\n" ++ e
    Right _ -> return ()

parseTanachHeader :: Test
parseTanachHeader = testParse "tanach header" $ loadParse Paths.headerFilePath header emptyLog

parseAllTanach :: Test
parseAllTanach = buildTestBracketed $ do
  result <- loadParse Paths.indexFilePath index emptyLog
  let
    tests = case result of
      Left x -> [testCase "parseAllTanach failure" $ assertFailure $ "index parse failure:\n" ++ x]
      Right idx -> fmap (\x -> testParse x (loadParse x tanach emptyLog)) $ Paths.getAllFilePaths idx
  return (testGroup "parseAllTanach" tests, return ())

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
  , testGroup "Tanach Header" [ parseTanachHeader, parseAllTanach ]
  ]
