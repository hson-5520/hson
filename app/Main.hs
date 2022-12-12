module Main where

import Data.Maybe qualified as Maybe
import FromJSON (parseJSON)
import FromJSONSchema (hsonToHSONSchema)
import ToJSON ()
import ValidateHSON (validateHSON)

main :: IO ()
main = do
  putStrLn "\nWELCOME TO HSON: HASKELL'S JSON!"
  putStrLn "THIS SHELL WILL ALLOW YOU TO VALIDATE A JSON OBJECT. \n"
  validateJSON

validateJSON :: IO ()
validateJSON = do
  putStrLn "ENTER THE FILE PATH FOR THE JSON SCHEMA"
  x <- getLine
  putStrLn "\nENTER THE FILE PATH FOR THE JSON OBJECT"
  y <- getLine
  validationResult <- p x y
  validateJSON
  where
    p schema obj = do
      s <- parseJSON schema
      o <- parseJSON obj
      case (s, o) of
        (Right x, Right y) -> do
          case validateHSON y (Maybe.fromJust $ hsonToHSONSchema x) of
            Right x -> putStrLn "\nVALIDATION PASSED!\n"
            Left y -> do
              putStrLn $ "\nVALIDATION FAILED: " ++ y ++ "\n"
        (Left err, Right y) -> putStrLn $ "\nPARSING JSON SCHEMA FAILED: " ++ err ++ "\n"
        (Right x, Left err) -> putStrLn $ "\nPARSING JSON OBJECT FAILED: " ++ err ++ "\n"
        (Left err1, Left err2) -> do
          putStrLn "\nPARSING JSON SCHEMA AND OBJECT FAILED!"
          putStrLn $ "Schema Error: " ++ err1
          putStrLn $ "Object Error: " ++ err2 ++ "\n"

-- ../test/json-schema/schema/address-schema.json
-- ../test/json-schema/object/address-object.json
-- ../test/json-schema/object/card-object.json

-- ../test/json/invalid/boolean.txt