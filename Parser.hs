module Parser
       ( Parser.parse
       ) where
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec

import Types

parseNumber = do
  sign <- option "" (string "-")
  value <- try parseFloat <|> parseInt
  return $ Number . read $ sign ++ value
    where parseInt = many1 digit
          parseFloat = do
            value <- option "0" (many1 digit)
            char '.'
            decimal <- many1 digit
            return $ value ++ "." ++ decimal

parseString = do
  char '"'
  str <- many strChars
  char '"'
  return $ String str
    where strChars = noneOf "\""

parseBoolean = do
  char '#'
  value <- oneOf "tf"
  return $ if value == 't' then Boolean True else Boolean False

parseSymbol = do
  name <- many (letter <|> digit <|> oneOf "+-*/_\\=!@#$%^&{}?")
  return $ Symbol name

parseList = do
  char '('
  optional spaces
  contents <- parseAnyExpr `sepEndBy` (many1 space)
  char ')'
  return $ fromList contents
  
parseQuote = do
  char '\''
  expr <- parseAnyExpr
  return $ Pair (Symbol "quote") (Pair expr Null)

parseAnyExpr = try parseList
               <|> try parseQuote
               <|> try parseNumber
               <|> try parseString
               <|> try parseBoolean
               <|> try parseSymbol

parseExpr = do
  optional spaces
  expr <- parseAnyExpr
  optional spaces
  eof
  return expr

parse :: String -> ThrowsError LispValue
parse input = case Text.ParserCombinators.Parsec.parse parseExpr "lisp" input of
  Right result -> return $ result
  Left error -> throwError . ParseError . show $ error
