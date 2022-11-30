module FromJSON where

import Control.Monad.Except
import Data.Map
import HSON
import Parser qualified as P

wsP :: Parser a -> Parser a
wsP p = p <* many P.space -- consume fst char & all whitespace until next char

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = let sP = P.string s in Control.Monad.void (wsP sP)

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP s val = let sP = P.string s in (val <$ wsP sP)

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]

-- Right [IntVal 1,IntVal 2,IntVal 3]
intValP :: Parser Value
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = P.choice [constP "true" (BoolVal True), constP "false" (BoolVal False)]

-- >>> P.parse (many nilValP) "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]
nilValP :: Parser Value
nilValP = constP "nil" NilVal

stringValP :: Parser Value
stringValP = StringVal <$> (P.char '\"' *> many (P.satisfy (/= '\"')) <* wsP (P.char '\"'))

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

expP :: Parser Expression
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Concat)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      tableConstP
        <|> Var <$> varP
        <|> parens expP
        <|> Val <$> valueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>>  P.parse (many varP) "x y z"
-- Right [Name "x", Name "y", Name "z"]

-- >>> P.parse varP "(x.y[1]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z")
varP :: Parser Var
varP = mkVar <$> prefixP <*> some indexP <|> Name <$> nameP
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = parens expP <|> Var . Name <$> nameP

    indexP :: Parser (Expression -> Var)
    indexP =
      flip Dot <$> (P.string "." *> nameP)
        <|> flip Proj <$> brackets expP

reserved :: [String]
reserved =
  [ "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "goto",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while"
  ]

-- >>> P.parse (many nameP) "x sfds _ 123abc nil"
-- Right ["x","sfds","_"]

nameP :: Parser Name
nameP = P.filter (`notElem` reserved) helperName

helperName :: Parser Name
helperName =
  (++)
    <$> some (P.choice [P.alpha, P.satisfy (== '_')])
    <*> many (P.choice [P.alpha, P.digit, P.satisfy (== '_')])
    <* many P.space

-- >>> P.parse (many uopP) "- - # not # - + #"
-- Right [Neg,Neg,Len,Not,Len,Neg]
uopP :: Parser Uop
uopP = P.choice [constP "-" Neg, constP "not" Not, constP "#" Len]

-- >>> P.parse (many bopP) "+ >= .. > == <= < - * % //"
-- Right [Plus,Ge,Concat,Gt,Eq,Le,Lt,Minus,Times,Modulo,Divide]
bopP :: Parser Bop
bopP =
  P.choice
    [ constP "//" Divide,
      constP "==" Eq,
      constP ">=" Ge,
      constP "<=" Le,
      constP ".." Concat,
      constP "+" Plus,
      constP "-" Minus,
      constP "*" Times,
      constP "%" Modulo,
      constP ">" Gt,
      constP "<" Lt
    ]

-- >>> P.parse tableConstP "{x=2}"
-- Right (TableConst [FieldName "x" (Val (IntVal 2))])

-- >>> P.parse tableConstP "{ x = 2, [3] = false }"
-- Right (TableConst [FieldName "x" (Val (IntVal 2)),FieldKey (Val (IntVal 3)) (Val (BoolVal False))])

tableConstP :: Parser Expression
tableConstP = TableConst <$> braces (P.sepBy (tableHelper) (wsP (P.char ',')))

--- >>> P.parse tableHelper "[3] = false"
-- Right (FieldKey (Val (IntVal 3)) (Val (BoolVal False)))

tableHelper :: Parser TableField
tableHelper =
  P.choice
    [ FieldName <$> nameP <*> (wsP (P.char '=') *> (wsP expP)),
      FieldKey <$> (brackets expP) <*> (wsP (P.char '=') *> (wsP expP))
    ]

statementP :: Parser Statement
statementP =
  P.choice
    [ constP ";" Empty,
      Assign <$> wsP varP <*> (wsP (P.char '=') *> (wsP expP)),
      If
        <$> (wsP (P.string "if") *> (wsP expP))
        <*> (wsP (P.string "then") *> (wsP blockP))
        <*> (wsP (P.string "else") *> (wsP blockP))
        <* (wsP (P.string "end")),
      While
        <$> (wsP (P.string "while") *> (wsP expP))
        <*> (wsP (P.string "do") *> (wsP blockP))
        <* (wsP (P.string "end")),
      Repeat
        <$> (wsP (P.string "repeat") *> (wsP blockP))
        <*> (wsP (P.string "until") *> (wsP expP))
    ]

blockP :: Parser Block
blockP = Block <$> many statementP

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

-- | Function to parse a JSON file and return an HSON object
parseJSON :: String -> IO (Either P.ParseError HSON)
parseJSON = undefined
