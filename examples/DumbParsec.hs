{-
 - Note: module Control.Monad.Trans.StateStrict is from package `transformers`
 -
 - install: cabal install --lib transformers
 - compile: ghc -package transformers DumbParsec.hs
 - or in ghci:   :set -package transformers
 -}
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Char

-- the definition of the parser type: It keeps a global String "rest of stuff
-- to parse", and maybe a result. (If the result is Nothing, it means the
-- parsing has failed.)
newtype Parser a =
  Parser (State String (Maybe a))

instance Functor Parser where
  fmap f (Parser s) = Parser $ fmap (fmap f) s

instance Applicative Parser where
  pure = Parser . pure . pure
  Parser l <*> Parser r = Parser $ liftA2 (<*>) l r

instance Monad Parser where
  return = pure
  Parser l >>= p =
    Parser $ do
      a <- l
      case a of
        Nothing -> return Nothing
        Just a ->
          let Parser r = p a
           in r

-- Alternative class is for things that may provide alternative results. E.g.,
-- if you have 2 Maybes and only need one result, one of them may be Nothing
-- and you can still collect the result from the other Just.
--
-- For parsers, (p1 <|> p2) returns the result from whichever of p1 and p2 has
-- succeeded.
instance Alternative Parser where
  empty = Parser $ pure empty
  Parser l <|> Parser r =
    Parser $ do
      backup <- get
      l' <- l
      case l' of
        Nothing -> put backup >> r
        a -> return a

-- parser that always fails
parseFail = empty

-- parser that retrieves the current "leftover stuff to parse"
parseGet = Parser $ Just <$> get

-- parser that doesn't parse anything but sets the leftover
parseSet s = Parser $ Just <$> put s

-- parser that succeeds only if there's no lefrovers
pEof = do
  s <- parseGet
  case s of
    "" -> return ()
    _ -> parseFail

-- helper for running the parser
doParse (Parser p) = runState p

-- helper for running the parser and ensuring the whole input was consumed
doParseEof p = fst . doParse (p >>= \r -> pEof >> return r)

-- a more applicative version of the previous
--
-- (intuition: operator <* is "pointing to the result that we want" and we
-- ignore the other one; but both side effects are executed.)
doParseEofA p = fst . doParse (p <* pEof)

-- parses any character (only fails if there are no characters left)
pAny = do
  s <- parseGet
  case s of
    (i:ss) -> parseSet ss >> return i
    _ -> parseFail

-- parses a character that satisfies some given condition
pCharCond f = do
  c <- pAny
  if f c
    then return c
    else parseFail

-- parses a character out of a given set of choices
pOneOf cs = pCharCond (`elem` cs)

-- parses anything except the given characters
pAnyExcept cs = pCharCond $ not . (`elem` cs)

-- parses one exact character
pChar c = pCharCond (== c)

-- parses a whole string of characters
pStr = traverse pChar

-- executes the parser `p` many times (at least once) and collects all results
-- into a list
pMany1 p = do
  x <- p
  xs <- pMany p
  return (x : xs)

-- executes the parser zero to infinity times.
pMany p = pMany1 p <|> pure []

-- parses any amount of blank characters
whitespace = pMany $ pCharCond isSpace

-- convers a parser to one which can ignore the initial whitespace
lexeme = (whitespace >>)

-- parses out a fixed string "keyword" with possible whitespace on the
-- beginning
lexStr = lexeme . pStr

-- parses something (p) surrounded by stuff parsed by l and r
pBracketed l r p = do
  l
  res <- p
  r
  return res

-- again, a more applicative variant of the previous
-- intuition: arrows again point to the thing that we want
pBracketedA l r p = l *> p <* r

-- parses something delimited by strings
pDelim l r = pBracketed (pStr l) (pStr r)

pBrackets = pBracketed (lexStr "[") (lexStr "]")

pBraces = pBracketed (lexStr "{") (lexStr "}")

-- quotes (the pAnyExcept is required to avoid quotes in quotes)
pQuoted q = pDelim q q . pMany $ pAnyExcept q

-- this is a nice helper for creating lists applicatively
-- ("join a list out of boxed things")
infixr 4 <:>
a <:> b = (:) <$> a <*> b
-- Example: Just 1 <:> Just [3,4] == Just [1,3,4]

-- parse stuff separated by a separator `sep`
pSep1 sep p = p <:> (sep >> pSep1 sep p) <|> (: []) <$> p

pSep sep p = pSep1 sep p <|> return []

pCommaDelimited = pSep (lexeme $ pChar ',')

-- This is not a complete JSON but close enough for our purposes
data JSON
  = JSInt Int
  | JSStr String
  | JSList [JSON]
  | JSObj [(String, JSON)]
  deriving (Show)

-- EXERCISE: JSON can also contain a "null" value; try to add it to the type
-- and parser.
pJSON = pJSInt <|> pJSStr <|> pJSList <|> pJSObj

pJSInt = JSInt . read <$> lexeme (pMany1 $ pOneOf ['0' .. '9'])

pJSStr' = lexeme (pQuoted "\"" <|> pQuoted "'")

pJSStr = JSStr <$> pJSStr'

pJSList = JSList <$> pBrackets (pCommaDelimited pJSON)

pJSObj = JSObj <$> pBraces (pCommaDelimited objItem)
  where
    objItem = do
      key <- pJSStr'
      lexeme $ pChar ':'
      (,) key <$> pJSON

-- demo parses some JSON. Note that we actually support ignoring blanks.
demo = doParseEof pJSON "[1, 2, {'a':123, 'b':[543]}]"
