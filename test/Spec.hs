import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Greek

main :: IO ()
main = defaultMain greekGroups
