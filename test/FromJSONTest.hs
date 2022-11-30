import Control.Applicative
import FromJSON
import HSON
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), runTestTT, (~?=))
import Test.QuickCheck

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

--- >>> runTestTT test_wsP

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (String "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (String "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [String "a", String "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [String " a", String "b"]
    ]