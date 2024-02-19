-- >>> parseChar "abc"
-- Just ("bc", 'a')
-- >>> parseChar ""
-- Nothing
parseChar :: String -> Maybe (String, Char)
parseChar "" = Nothing
parseChar (c:rest) = Just (rest, c)

-- >>> parseInt "123+456"
-- Just ("+456", 123)
-- >>> parseInt "xyz456""
-- Nothing
parseInt :: String -> Maybe (String, Int)
parseInt s = case reads s of
  [(x, rest)] -> Just (rest, x)
  _           -> Nothing

newtype Parser a = Parser (String -> Maybe (String, a))

char :: Parser Char
char = Parser parseChar

int :: Parser Int
int = Parser parseInt

-- >>> parse int "123+456"
-- Just ("+456", 123)
parse :: Parser a -> String -> Maybe (String, a)
parse (Parser p) = p

is :: Char -> Parser Char
is c = Parser $
  \inputString -> case parse char inputString of
      Just (rest, result) | result == c -> Just (rest, result)
      _                                 -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser (((f <$>) <$>) <$> p)

instance Applicative Parser where
  pure a = Parser (\b -> Just (b, a))

  (Parser f) <*> (Parser g) = Parser $
    \i -> case f i of
            Just (r1, p1) -> case g r1 of
                                Just (r2, p2) -> Just (r2, p1 p2)
                                Nothing       -> Nothing
            Nothing       -> Nothing


main :: IO()
main = do
  print $ parse (is '+') "+456"
  print $ parse ((+1)<$>int) "1bc"
  let charIntPairParser = (,) <$> char <*> int
  print $ parse charIntPairParser "a12345b"
