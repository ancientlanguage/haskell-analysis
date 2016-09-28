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

main :: IO ()
main = defaultMain
  [ testGroup "SBLGNT" [ parseSblgnt ]
  ]
