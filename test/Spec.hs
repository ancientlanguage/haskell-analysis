import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Sblgnt.Parser as Sblgnt
import qualified Xml.Events as Xml
import qualified Xml.Parser as Xml

parseSblgnt = testCase "parse sblgnt" $ do
  let path = "./data/xml-sblgnt/sblgnt.xml"
  Xml.readRootElement path >>= \case
    Left e -> assertFailure $ "Xml.readRootElement:\n" ++ show e
    Right root -> case Xml.parseRoot path Sblgnt.sblgnt root of
      Left e -> assertFailure $ "Xml.parseRoot:\n" ++ e
      Right x -> return ()

main :: IO ()
main = defaultMain
  [ testGroup "SBL GNT" [parseSblgnt]
  ]
