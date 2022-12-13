module Main where

import Data.Maybe qualified as Maybe
import FromJSON (parseJSON)
import FromJSONSchema (hsonToHSONSchema)
import HSONSchema
import ToJSON (hsonToString)
import ValidateHSON (validateHSON)

-- | main function where the execution of the program begins
main :: IO ()
main = do
  putStrLn "\nWELCOME TO HSON: HASKELL'S JSON!"
  putStrLn "THIS SHELL WILL ALLOW YOU TO VALIDATE A JSON OBJECT. \n"
  validateJSON

-- | function that validates a JSON object against a JSON schema
validateJSON :: IO ()
validateJSON = do
  putStrLn "ENTER THE FILE PATH FOR THE JSON SCHEMA"
  schemaFilePath <- getLine
  hsonSchema <- validateSchema schemaFilePath
  case hsonSchema of
    Nothing -> validateJSON
    Just hs -> do
      print hs
      putStrLn "\nENTER THE FILE PATH FOR THE JSON OBJECT"
      objectFilePath <- getLine
      validationResult <- p hs objectFilePath
      validateJSON
  where
    p schema obj = do
      o <- parseJSON obj
      case o of
        (Right hson) -> case validateHSON hson schema of
          Right x -> putStrLn "\nVALIDATION PASSED!\n"
          Left y -> do
            putStrLn $ "\nVALIDATION FAILED: " ++ y ++ "\n"
        (Left err) -> putStrLn $ "\nPARSING JSON OBJECT FAILED: " ++ err ++ "\n"

-- | helper function to parse a JSON file into HSON and then HSONSchema
validateSchema :: String -> IO (Maybe HSONSchema)
validateSchema schema = do
  hson <- parseJSON schema
  case hson of
    Left err -> do
      putStrLn $ "\nPARSING JSON FILE FAILED: " ++ err ++ "\n"
      return Nothing
    Right hs -> case hsonToHSONSchema hs of
      Left s -> do
        putStrLn $ "\nPARSING JSON SCHEMA FAILED: " ++ s ++ "\n"
        return Nothing
      Right hsonSchema -> return $ Just hsonSchema

-- ../test/json-schema/schema/address-schema.json
-- ../test/json-schema/object/address-object.json
-- ../test/json-schema/object/card-object.json

-- test/json-schema/schema/coordinate-schema.json
-- test/json-schema/object/coordinate-object.json

-- ../test/json/invalid/boolean.txt