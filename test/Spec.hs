import FromJSONTest (test_fromJSON)
import Lib
import Test.HUnit
import Test.QuickCheck
import ToHSONSchemaTest (test_validation)
import ToJSONTest (test_toJSON)

main :: IO ()
main = do
  putStrLn "FromJSON Tests"
  test_fromJSON
  putStrLn "ToJSON Tests"
  test_toJSON
  putStrLn "Validation Tests"
  putStrLn someFunc
  test_validation
