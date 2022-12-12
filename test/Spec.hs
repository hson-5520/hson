import FromJSONTest (test_fromJSON)
import HSONTest (prop_hsonroundtrip)
import HSONValidatorTest (test_validateSchemas)
import Lib
import Test.HUnit
import Test.QuickCheck
import ToHSONSchemaTest (test_toHSONSchema)
import ToJSONTest (test_toJSON)

main :: IO ()
main = do
  putStrLn "FromJSON Tests"
  test_fromJSON
  putStrLn "ToJSON Tests"
  test_toJSON
  putStrLn "QuickCheck Round Trip HSON Tests"
  quickCheck prop_hsonroundtrip
  putStrLn "Converting HSON to HSON Schema Tests"
  test_toHSONSchema
  putStrLn "Validate HSON with HSON Schema Tests"
  test_validateSchemas
  putStrLn someFunc
