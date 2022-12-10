import FromJSONTest (test_fromJSON)
import Lib
import Test.HUnit
import Test.QuickCheck
import ToJSONTest (test_toJSON)

main :: IO ()
main = do
  putStrLn "FromJSON Tests"
  test_fromJSON
  putStrLn "ToJSON Tests"
  test_toJSON
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"
