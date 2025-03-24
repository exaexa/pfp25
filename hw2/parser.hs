import Data.Char (isDigit)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

addExpr, multExpr, baseExpr, integer, expr :: (Num a, Read a) => Parser a
addExpr = try ((+) <$> multExpr <* char '+' <*> addExpr) <|> multExpr
multExpr = try ((*) <$> baseExpr <* char '*' <*> multExpr) <|> baseExpr
baseExpr = choice [char '(' *> addExpr <* char ')', integer]
integer = read <$> takeWhile1P Nothing isDigit
expr = addExpr

main =
  case parseMaybe expr "(1+2)*3" of
    Just res -> print (res :: Integer)
    Nothing -> putStrLn "parsing failed"
