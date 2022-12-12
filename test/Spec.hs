import FromJSONSchemaTest (test_toHSONSchema)
import FromJSONTest (test_fromJSON)
import HSONTest (prop_hsonroundtrip)
import Test.HUnit
import Test.QuickCheck
import ToJSONTest (test_toJSON)
import ValidateHSONTest (test_validateSchemas)

main :: IO ()
main = do
  putStrLn "FromJSON Tests"
  test_fromJSON
  putStrLn "ToJSON Tests"
  test_toJSON
  putStrLn "HSON Tests: QuickCheck"
  quickCheck prop_hsonroundtrip
  putStrLn "FromJSONSchema Tests"
  test_toHSONSchema
  putStrLn "ValidateHSON Tests"
  test_validateSchemas
  putStrLn "End of tests"
