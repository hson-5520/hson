module Main where

import Data.Maybe qualified as Maybe
import FromJSON
import FromJSONSchema
import Lib
import ToJSON
import ValidateHSON

main :: IO ()
main = do
  putStrLn "\nWelcome to HSON: Haskell's JSON!"
  putStrLn "This shell will allow you to validate a JSON object. \n"
  putStrLn "Enter the file path for the JSON Schema"
  x <- getLine
  putStrLn "Enter the file path for the JSON Object"
  y <- getLine
  validationResult <- p x y
  if validationResult
    then putStrLn "Success"
    else putStrLn "Validation failed"
  where
    p schema obj = do
      s <- parseJSON schema
      o <- parseJSON obj
      case (s, o) of
        (Right x, Right y) -> do
          return $ validateHSON y (Maybe.fromJust $ hsonToHSONSchema x)
        (_, _) -> return False

-- ../test/json-schema/schema/address-schema.json
-- ../test/json-schema/object/address-object.json