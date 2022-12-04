module FromJSON where

import Control.Applicative
import Control.Monad qualified
import Control.Monad.Except
import Data.Char qualified as P
import Data.Map
import Data.Map qualified as Map
import HSON (HSON, Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import Parser
import Parser qualified as P

---------------------------- Parsing Primatives --------------------------------

-- | consume fst char & all whitespace until next char
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- | creates a parser for a specific input string
stringP :: String -> Parser ()
stringP s = let sP = P.string s in Control.Monad.void (wsP sP)

-- | parses and replaces one occurrence of the input string with a specific value
constP :: String -> a -> Parser a
constP s val = let sP = P.string s in (val <$ wsP sP)

-- | parses a string appearing between two parentheses
parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

-- | parses a string appearing between two curly braces
braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- | parses a string appearing between two brackets
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

combineAsTuple :: a -> b -> (a, b)
combineAsTuple x y = (x, y)

-------------------------- Key Parsing -----------------------------------------

-- | parses a JSON key and the trailing colon
keyP :: Parser Key
keyP = P.char '\"' *> many (P.satisfy (/= '\"')) <* wsP (P.char '\"') <* wsP (P.char ':')

--------------------------------- Value Parsing --------------------------------

-- | parses any value associated with a key in a JSON object
valueP :: Parser Value
valueP =
  P.choice
    [ stringValP,
      numberValP,
      intValP,
      booleanValP,
      arrayValP,
      objectValP,
      nullValP
    ]

-- | parses any string
stringValP :: Parser Value
stringValP = String <$> (P.char '\"' *> many (P.satisfy (/= '\"')) <* wsP (P.char '\"'))

-- | parses any integer
intValP :: Parser Value
intValP = Integer <$> P.int

-- | parses a negative double
negativeDecimalParser :: Parser Value
negativeDecimalParser = Number . read <$> ((++) <$> ((++) <$> ((++) <$> string "-" <*> some digit) <*> string ".") <*> some digit)

-- | parses a positive double
positiveDecimalParser :: Parser Value
positiveDecimalParser = Number . read <$> ((++) <$> ((++) <$> some digit <*> string ".") <*> some digit)

-- | parses any number
numberValP :: Parser Value
numberValP = P.choice [negativeDecimalParser, positiveDecimalParser]

-- | parses any boolean value
booleanValP :: Parser Value
booleanValP = P.choice [constP "true" (Boolean True), constP "false" (Boolean False)]

-- | parses any list which appears as a value
arrayValP :: Parser Value
arrayValP = Array <$> brackets (P.sepBy valueP (wsP (char ',')))

-- | parses an entire JSON object into HSON
objectValP :: Parser Value
objectValP = Object <$> hsonP

-- | parses a null value
nullValP :: Parser Value
nullValP = constP "null" Null

---------------------------- Parse JSON ----------------------------------------------

-- | parses a single item (key, value) in a JSON file
itemP :: Parser (Key, Value)
itemP = combineAsTuple <$> wsP keyP <*> wsP valueP

-- | parses an entire JSON file into an HSON object
hsonP :: Parser HSON
hsonP = braces (sepBy itemP (wsP (char ',')))

-- | takes a JSON file and returns an HSON object
parseJSON :: String -> IO (Either P.ParseError HSON)
parseJSON = P.parseFromFile (const <$> hsonP <*> P.eof)
