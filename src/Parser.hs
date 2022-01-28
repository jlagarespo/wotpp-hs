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
expr = (prefixApp <|> match <|> literal <|> builtin) `chainl1` cat

prefixApp :: Parser Expr
prefixApp = do
  id <- identifier
  args <- option [] $ parens $ commaSep expr
  pure $ EApp id args

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

literal :: Parser Expr
literal = lexeme $ try $ between (char '"') (char '"') (fullStr $ satisfy (`notElem` ("\"\\"::String)))
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
      id <- (L.singleton <$> oneOf "\\\",&") <|> identifier
      params <-
        if id /= "&"
          then option [] $ braces $ commaSep $ fullStr $ satisfy (`notElem` ("\\,}"::String))
          else pure []

      pure $
        if not $ null params
        then EApp id params
        else case id of
               "a"  -> ELit "\a"
               "b"  -> ELit "\b"
               "f"  -> ELit "\f"
               "n"  -> ELit "\n"
               "r"  -> ELit "\r"
               "t"  -> ELit "\t"
               "v"  -> ELit "\v"
               "\\" -> ELit "\\"
               "\"" -> ELit "\""
               ","  -> ELit ","
               "&"  -> ELit ""
               _    -> EApp id []

infixOp :: Text -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = symbol x >> pure f

cat :: Parser (Expr -> Expr -> Expr)
cat = infixOp ".." ECat

builtin :: Parser Expr
builtin = do
  symbol "__builtin"
  name <- identifier
  args <- option [] $ parens $ commaSep expr
  pure $ EBuiltin name args

patt :: Parser Pattern
patt = (pattWild <|> pattLit) `chainl1` pattCat
  where
    -- pattExpr = PExpr <$> expr
    pattLit = literal >>= \case
      ELit x -> pure $ PLit x
      _      -> unexpected "non-builtin escapes (which are disallowed in pattern literals until I find a way to evaluate them.)"
    pattWild = PWild <$> identifier
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

parseDocument :: Text -> Text -> Either Error [Statement]
parseDocument x sourceName =
  case parse document (L.unpack sourceName) x of
    Left err -> Left $ ParseErr err
    Right x  -> Right x
