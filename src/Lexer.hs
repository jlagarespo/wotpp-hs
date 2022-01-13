module Lexer where

import Data.Char (isSymbol)
import Data.List (isInfixOf)
import Text.Parsec

import Text.Parsec.Text.Lazy (Parser)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.Functor.Identity (Identity)

import AST

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

stringLiteral :: Parser Expr
stringLiteral = lexeme $ try $ between (char '"') (char '"') (fullStr $ satisfy (`notElem` ("\"\\"::String)))
  where
    fullStr :: Parser Char -> Parser Expr
    fullStr char = do
      parts <- many (strEscape <|> strSection char)
      if null parts
        then pure $ ELit ""
        else pure $ foldl1 ECat parts

    strSection :: Parser Char -> Parser Expr
    strSection char = do
      str <- many1 char
      pure $ ELit $ L.pack str

    strEscape :: Parser Expr
    strEscape = do
      char '\\'
      id <- identifier
      params <- option [] $ braces $ commaSep $ fullStr $ satisfy (`notElem` ("\\,}"::String))

      case id of
        "a"  -> pure $ ELit "\a"
        "b"  -> pure $ ELit "\b"
        "f"  -> pure $ ELit "\f"
        "n"  -> pure $ ELit "\n"
        "r"  -> pure $ ELit "\r"
        "t"  -> pure $ ELit "\t"
        "v"  -> pure $ ELit "\v"
        "\\" -> pure $ ELit "\\"
        "\"" -> pure $ ELit "\""
        "'"  -> pure $ ELit "'"
        "&"  -> pure $ ELit ""
        _    -> pure $ EApp id params

comment :: ParsecT Text () Identity ()
comment = symbol "#[" >> inComment
  where
    inComment = (symbol "]" >> pure ())
            <|> (comment >> inComment)
            <|> (skipMany (noneOf startEnd) >> inComment)
            <|> (oneOf startEnd >> inComment)

    start = "#["
    end = "]"
    startEnd = start <> end
