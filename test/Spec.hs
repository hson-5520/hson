import FromJSONTest (test_fromJSON)
import HSONTest (prop_hsonroundtrip)
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
  putStrLn "QuickCheck Round Trip HSON Tests"
  quickCheck prop_hsonroundtrip
  putStrLn "Validation Tests"
  test_validation
  putStrLn someFunc
