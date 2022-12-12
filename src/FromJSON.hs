module FromJSON where

import Control.Applicative
import Control.Monad qualified
import Control.Monad qualified as Monad
import Control.Monad.Except
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import Parser (Parser)
import Parser qualified as P

---------------------------- Parsing Primitives --------------------------------

-- | consume fst char & all whitespace until next char
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- | creates a parser for a specific input string
stringP :: String -> Parser ()
stringP s = let sP = P.string s in Control.Monad.void (wsP sP)

-- | parses and replaces one occurrence of the input string with a specific value
constP :: String -> a -> Parser a
constP s val = let sP = P.string s in (val <$ wsP sP)

-- | parses a string appearing between two curly braces
braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- | parses a string appearing between two brackets
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

-- | parses either a negative character or no character
signParser :: Parser String
signParser = P.choice [P.string "-", P.string ""]

-- | parses the scientific notation symbols
scientificNotationParser :: Parser String
scientificNotationParser =
  P.choice
    [ P.string "e-",
      P.string "E-",
      P.string "e+",
      P.string "E+",
      P.string "e",
      P.string "E"
    ]

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

stringValP :: Parser Value
stringValP = String <$> (P.char '\"' *> many (P.satisfy (/= '\"')) <* wsP (P.char '\"'))

-- | parses an integer
intValP :: Parser Value
intValP = Integer <$> wsP P.int

-- | parses an integer in scientific notation
scientificIntParser :: Parser Value
scientificIntParser =
  Number
    . read
    <$> ( (++)
            <$> ( (++)
                    <$> ((++) <$> signParser <*> wsP (some P.digit))
                    <*> wsP scientificNotationParser
                )
            <*> wsP (some P.digit)
        )

-- >>> doParse numberValP "1 "
-- Just (Integer 1,"")

-- | parses a double that is positive or negative and can be in scientific notation
decimalParser :: Parser Value
decimalParser =
  Number . read
    <$> ( (++)
            <$> ( (++)
                    <$> ((++) <$> ((++) <$> signParser <*> some P.digit) <*> P.string ".")
                    <*> some P.digit
                )
            <*> ((++) <$> scientificNotationParser <*> wsP (some P.digit) <|> wsP (P.string ""))
        )

-- | parses any number
numberValP :: Parser Value
numberValP = P.choice [decimalParser, scientificIntParser]

-- | parses any boolean value
booleanValP :: Parser Value
booleanValP = P.choice [constP "true" (Boolean True), constP "false" (Boolean False)]

-- | parses any list which appears as a value
arrayValP :: Parser Value
arrayValP = Array <$> brackets (P.sepBy valueP (wsP (P.char ',')))

-- | parses an entire JSON object into HSON
objectValP :: Parser Value
objectValP = Object <$> hsonP

-- | parses a null value
nullValP :: Parser Value
nullValP = constP "null" Null

---------------------------- Parse JSON ----------------------------------------------

-- | parses a single item (key, value) in a JSON file
itemP :: Parser (Key, Value)
itemP = (,) <$> wsP keyP <*> wsP valueP

-- | parses an entire JSON file into an HSON object
hsonP :: Parser HSON
hsonP = H <$> braces (P.sepBy itemP (wsP (P.char ',')))

-- | takes a JSON file and returns an HSON object
parseJSON :: String -> IO (Either P.ParseError HSON)
parseJSON = P.parseFromFile (const <$> hsonP <*> P.eof)
