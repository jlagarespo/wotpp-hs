 module Lexer where

import Data.Char (isSymbol)
import Data.List (isInfixOf)
import Text.Parsec
-- import Text.Parsec.Char
-- import qualified Text.Parsec.Token as Tok
-- import Text.Parsec.Token (LanguageDef, GenLanguageDef(..), makeTokenParser)
import Text.Parsec.Text.Lazy (Parser)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.Functor.Identity (Identity)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  futile
  pure x

  where
    futile = skipMany $ whitespace <|> comment
    whitespace = space >> pure ()

symbol = (L.pack<$>) . lexeme . try . string . L.unpack

identifier :: Parser Text
identifier = lexeme $ try $ do
  str <- many1 identChar
  if isKeyword str
    then unexpected ("reserved token " <> str)
    else pure $ L.pack str

  where
    identChar = alphaNum <|> digit <|> oneOf "'_"
    keywords = ["let", "match", "to"]
    isKeyword = (`elem` keywords)

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = (`sepBy` symbol ",")
commaSep1 = (`sepBy1` symbol ",")

stringLiteral :: Parser Text
stringLiteral = lexeme $ try $ do
  str <- between (char '"') (char '"') (many $ strEscape <|> strChar)
  pure $ L.pack str

  where
    strChar, strEscape :: Parser Char
    strChar = satisfy (/= '"')
    strEscape = do
      char '\\'
      v <- choice $ map char "nrtb"
      case v of
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        'b' -> pure '\b'
        _   -> undefined

comment = symbol "#[" >> inComment
  where
    inComment = (symbol "]" >> pure ())
            <|> (comment >> inComment)
            <|> (skipMany (noneOf startEnd) >> inComment)
            <|> (oneOf startEnd >> inComment)

    start = "#["
    end = "]"
    startEnd = start <> end
