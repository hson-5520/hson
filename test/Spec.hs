import FromJSONTest (test_all)
import Lib
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "FromJSON Tests"
  test_all
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"
