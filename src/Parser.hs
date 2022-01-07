module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer

type Identifier = String
data Expr = ELiteral String | EInvoke Identifier [Expr] | EConcat Expr Expr | EMatch [(Expr, Expr)] deriving Show
data Statement = SExpr Expr | SFunction Identifier [Identifier] Expr deriving Show

expr :: Parser Expr
expr = (invoke <|> literal <|> match) `chainl1` cat

literal :: Parser Expr
literal = ELiteral <$> stringLiteral

invoke :: Parser Expr
invoke = do
  id <- identifier
  args <- option [] $parens $ expr `sepBy` reservedOp ","
  pure $ EInvoke id args

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reservedOp x >> pure f

cat :: Parser (Expr -> Expr -> Expr)
cat = infixOp ".." EConcat

match :: Parser Expr
match = do
  reserved "match"
  cond <- expr
  branches <- many1 branch
  pure $ EMatch branches

  where
    branch = do
      l <- literal
      reservedOp "->"
      r <- expr
      pure (l, r)

statement :: Parser Statement
statement = (SExpr <$> expr) <|> function

function :: Parser Statement
function = do
  reserved "let"
  name <- identifier
  params <- option [] $ parens $ identifier `sepBy` reservedOp ","
  body <- expr
  pure $ SFunction name params body

document :: Parser [Statement]
document = many1 statement <* eof
