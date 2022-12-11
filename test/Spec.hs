import FromJSONTest (test_fromJSON)
import HSONSchemaTest (test_validation)
import Lib
import Test.HUnit
import Test.QuickCheck
import ToJSONTest (test_toJSON)

main :: IO ()
main = do
  putStrLn "Validation Tests"
  test_validation
  putStrLn "FromJSON Tests"
  test_fromJSON
  putStrLn "ToJSON Tests"
  test_toJSON
  putStrLn someFunc
