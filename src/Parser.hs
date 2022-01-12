module Parser where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import AST
import Error
import Lexer

-- TODO: Migrate to megaparsec for proper error recovery.
-- TODO: Implement warnings.

expr :: Parser Expr
expr = (app <|> literal <|> match) `chainl1` cat

literal :: Parser Expr
literal = ELit <$> stringLiteral

app :: Parser Expr
app = do
  id <- identifier
  args <- option [] $ parens $ commaSep expr
  pure $ EApp id args

infixOp :: Text -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = symbol x >> pure f

cat :: Parser (Expr -> Expr -> Expr)
cat = infixOp ".." ECat

match :: Parser Expr
match = do
  symbol "match"
  what <- expr
  symbol "to"
  branches <- braces $ many1 branch
  pure $ EMatch what branches

  where
    branch = do
      l <- patt
      symbol "->"
      r <- body
      pure (l, r)

patt :: Parser Pattern
patt = (pattLiteral <|> wildLiteral) `chainl1` pattCat
  where
    pattLiteral = PLit <$> stringLiteral
    wildLiteral = PWild <$> identifier
    pattCat = infixOp ".." PCat

statement :: Parser Statement
statement = (SExpr <$> expr) <|> function

function :: Parser Statement
function = do
  symbol "let"
  name <- identifier
  params <- option [] $ parens $ commaSep patt
  b <- body
  pure $ SFunction name params b

body :: Parser Body
body = braces block <|> (Body [] <$> expr)
  where
    block = do
      statements <- many1 statement
      -- TODO: Warn about useless non-tail expressions (no side effects.)
      -- TODO: If there are no statements, warn about redundant braces.
      case last statements of
        (SExpr tailExpr) -> pure $ Body (init statements) tailExpr
        _ -> fail "The last statement of a block must be an expression."

document :: Parser [Statement]
document = many statement <* eof

parseDocument :: Text -> Either Error [Statement]
parseDocument x = case parse document "<stdin>" x of
                    Left err -> Left $ ParseErr err
                    Right x  -> Right x
