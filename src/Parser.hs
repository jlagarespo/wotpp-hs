module Parser where

import Data.Text.Lazy (Text)
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import AST
import Lexer

-- TODO:
-- - Migrate to megaparsec for proper error recovery.
-- - Implement warnings.

expr :: Parser Expr
expr = (invoke <|> literal <|> match) `chainl1` cat

literal :: Parser Expr
literal = ELit <$> stringLiteral

invoke :: Parser Expr
invoke = do
  id <- identifier
  args <- option [] $ parens $ commaSep expr
  pure $ EApp id args

infixOp :: Text -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reservedOp x >> pure f

cat :: Parser (Expr -> Expr -> Expr)
cat = infixOp ".." ECat

match :: Parser Expr
match = do
  reserved "match"
  what <- expr
  reserved "to"
  branches <- many1 branch
  pure $ EMatch what branches

  where
    branch = do
      l <- literal
      reservedOp "->"
      r <- body
      pure (l, r)

statement :: Parser Statement
statement = (SExpr <$> expr) <|> function

function :: Parser Statement
function = do
  reserved "let"
  name <- identifier
  params <- option [] $ parens $ commaSep identifier
  b <- body
  pure $ SFunction name params b

body :: Parser Body
body = block <|> (Body [] <$> expr)
  where
    block = do
      reservedOp "{"
      statements <- many statement
      -- TODO: Warnings:
      -- - Warn about useless non-tail expressions (no side effects.)
      -- - If there are no statements, warn about redundant braces.
      tailExpr <- expr
      reservedOp "}"
      pure $ Body statements tailExpr

document :: Parser [Statement]
document = many1 statement <* eof
