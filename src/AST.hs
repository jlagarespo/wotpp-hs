module AST where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

type Identifier = Text
data Body = Body [Statement] Expr deriving Show
data Expr = ELit Text | EApp Identifier [Expr] | EBuiltin Identifier [Expr] | ECat Expr Expr | EMatch Expr [(Pattern, Body)] deriving Show
data Pattern = PLit Text | PWild Identifier | PCat Pattern Pattern deriving Show
data Statement = SExpr Expr | SFunction Identifier [Pattern] Body deriving Show

commaList :: [Text] -> Text
commaList [] = ""
commaList x = "(" <> L.intercalate ", " x <> ")"

showBlock :: [Text] -> Text
showBlock statements = "{\n" <> L.concat (map (\x -> "\t" <> x <> "\n") statements) <> "}"

showBody :: Body -> Text
showBody (Body [] expr) = showExpr expr
showBody (Body statements expr) = showBlock $ map showStatement statements <> [showExpr expr]

showExpr :: Expr -> Identifier
showExpr (ELit s) = "\"" <> s <> "\""
showExpr (EApp id args) = id <> commaList (map showExpr args)
showExpr (EBuiltin id args) = "__builtin" <> id <> commaList (map showExpr args)
showExpr (ECat l r) = showExpr l <> " .. " <> showExpr r
showExpr (EMatch what branches) = "match " <> showExpr what <> " to " <> showBlock (map (\(pat, body) -> showPattern pat <> " -> " <> showBody body) branches)

showPattern :: Pattern -> Text
-- showPattern (PExpr e) = showExpr e
showPattern (PLit s) = "\"" <> s <> "\""
showPattern (PWild x) = x
showPattern (PCat l r) = showPattern l <> " .. " <> showPattern r

showStatement :: Statement -> Identifier
showStatement (SExpr e) = showExpr e
showStatement (SFunction id params body) = "let " <> id <> commaList (map showPattern params) <> " " <> showBody body
