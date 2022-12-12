module Main where

import Data.Maybe qualified as Maybe
import FromJSON (parseJSON)
import FromJSONSchema (hsonToHSONSchema)
import Lib ()
import ToJSON ()
import ValidateHSON (validateHSON)

main :: IO ()
main = do
  putStrLn "\nWelcome to HSON: Haskell's JSON!"
  putStrLn "This shell will allow you to validate a JSON object. \n"
  validateJSON

validateJSON :: IO ()
validateJSON = do
  putStrLn "Enter the file path for the JSON Schema"
  x <- getLine
  putStrLn "Enter the file path for the JSON Object"
  y <- getLine
  validationResult <- p x y
  validateJSON
  where
    p schema obj = do
      s <- parseJSON schema
      o <- parseJSON obj
      case (s, o) of
        (Right x, Right y) -> do
          let validationResult = validateHSON y (Maybe.fromJust $ hsonToHSONSchema x)
          if validationResult
            then putStrLn "Validation Passed!"
            else putStrLn "Validation Failed!"
        (_, Right y) -> putStrLn "Parsing JSON Schema Failed"
        (Right x, _) -> putStrLn "Parsing JSON Object Failed"
        (_, _) -> putStrLn "Parsing JSON Schema and JSON Object Failed"

-- ../test/json-schema/schema/address-schema.json
-- ../test/json-schema/object/address-object.json
-- ../test/json-schema/object/card-object.json