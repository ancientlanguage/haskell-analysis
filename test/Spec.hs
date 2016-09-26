import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import qualified Sblgnt.Parser as Sblgnt
import qualified Xml.Events as Xml
import qualified Xml.Parser as Xml
import qualified Xml.PositionTypes as Xml
import Log

logBook :: Xml.Element -> IO ()
logBook = logElement (\x -> x == "book")

parseSblgnt :: Test
parseSblgnt = testCase "parse sblgnt" $ do
  let xmlPath = "./data/xml-sblgnt/sblgnt.xml"
  rootResult <- Xml.readRootElement logBook xmlPath
  case rootResult of
    Left e -> assertFailure $ "Xml.readRootElement:\n" ++ show e
    Right root -> case Xml.parseRoot xmlPath Sblgnt.sblgnt root of
      Left e -> assertFailure $ "Xml.parseRoot:\n" ++ e
      Right _ -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "SBLGNT" [parseSblgnt]
  ]
