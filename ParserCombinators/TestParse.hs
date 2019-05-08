-- | 

module Haskell.FirstPrinciples.State.TestParse where
import Text.Trifecta
-- import Text.Parser.Combinators -- must be added in ghc 8.6.3
-- type Parser a = String -> Maybe (a, String)
-- type Token = Char
-- newtype Parser a =
--   Parser ([Token] -> [(a, [Token])])
-- -- char :: Char -> Parser Char
-- char c =
--    Parser $ \s ->
--      case s of
--        (x:xs) -> if c == x
--                 then [(c, xs)]
--                 else []
--        _ -> []

-- instance Monad Parser s where
--   return a = State $ \s -> (a,s)
--   (State f) >>= g = State $ \s -> runState (g . fst $ f s) s
--   (State f) >> (State g) = State $ \s ->  g (snd $ f s)
one :: Parser Char
one = char '1'

stop :: Parser a
stop = unexpected "stop"

-- one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo"
  testParse oneTwo
  pNL "oneTwo'"
  testParse oneTwo'
