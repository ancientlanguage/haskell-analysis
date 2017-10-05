import System.FilePath ((</>))
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

parseTanachHeader :: FilePath -> Test
parseTanachHeader dataPath = testParse "tanach header" $ loadParse (Paths.headerFilePath dataPath) header emptyLog

parseAllTanach :: FilePath -> Test
parseAllTanach dataPath = buildTestBracketed $ do
  result <- loadParse (Paths.indexFilePath dataPath) index emptyLog
  let
    tests = case result of
      Left x -> [testCase "parseAllTanach failure" $ assertFailure $ "index parse failure:\n" ++ x]
      Right idx -> fmap (\x -> testParse x (loadParse x tanach emptyLog)) $ Paths.getAllFilePaths dataPath idx
  return (testGroup "parseAllTanach" tests, return ())

parseSblgnt :: FilePath -> Test
parseSblgnt dataPath = testParse "parse sblgnt" $ loadParse xmlPath sblgnt logBook
  where
  xmlPath = dataPath </> "xml-sblgnt/sblgnt.xml"

parsePerseusTeiEpidoc :: String -> Test
parsePerseusTeiEpidoc p = testParse ("parse perseus tei epidoc: " ++ p) $ loadParse p tei logBook

main :: IO ()
main =
  let dataPath = "../data"
  in defaultMain
  [ testGroup "Perseus" (fmap parsePerseusTeiEpidoc $ perseusShortList dataPath)
  , testGroup "SBLGNT" [ parseSblgnt dataPath ]
  , testGroup "Tanach Header" [ parseTanachHeader dataPath, parseAllTanach dataPath ]
  ]
