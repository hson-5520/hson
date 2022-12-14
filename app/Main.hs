module Main where

import Data.Maybe qualified as Maybe
import FromJSON (parseJSON)
import FromJSONSchema (fromJSONSchema, hsonToHSONSchema)
import HSONSchema
import ToJSON (hsonToString)
import ValidateHSON (validateHSON)

-- | main function where the execution of the program begins
main :: IO ()
main = do
  putStrLn "--------------------------------------------------------------------"
  putStrLn "\nWELCOME TO HSON: HASKELL'S JSON!"
  putStrLn "THIS SHELL WILL ALLOW YOU TO VALIDATE A JSON OBJECT. \n"
  putStrLn "--------------------------------------------------------------------"
  putStrLn "\nENTER THE FILE PATH FOR THE JSON SCHEMA"
  schemaFilePath <- getLine
  hsonSchema <- fromJSONSchema schemaFilePath
  case hsonSchema of
    Nothing -> main
    Just hs -> do
      print hs
      validateJSON hs

-- | function that validates a JSON object against a JSON schema
validateJSON :: HSONSchema -> IO ()
validateJSON hs = do
  putStrLn "--------------------------------------------------------------------"
  putStrLn "\nENTER THE FILE PATH FOR THE JSON OBJECT"
  objectFilePath <- getLine
  if objectFilePath == "r"
    then main
    else do
      validationResult <- p hs objectFilePath
      validateJSON hs
  where
    p schema obj = do
      o <- parseJSON obj
      case o of
        (Right hson) -> case validateHSON hson schema of
          Right x -> putStrLn "\nVALIDATION PASSED!\n"
          Left y -> do
            putStrLn $ "\nVALIDATION FAILED:\n" ++ y ++ "\n"
        (Left err) -> putStrLn $ "\nPARSING JSON OBJECT FAILED: " ++ err ++ "\n"