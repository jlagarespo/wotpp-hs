module AST where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

type Identifier = Text
data Body = Body [Statement] Expr
data Expr = ELit Text | EApp Identifier [Expr] | ECat Expr Expr | EMatch Expr [(Pattern, Body)]
data Pattern = PLit Text | PWild Identifier | PCat Pattern Pattern
data Statement = SExpr Expr | SFunction Identifier [Pattern] Body

commaList :: [Text] -> Text
commaList [] = ""
commaList x = "(" <> L.intercalate ", " x <> ")"

showBlock :: [Text] -> Text
showBlock statements = "{\n" <> L.concat (map (\x -> "\t" <> x <> "\n") statements) <> "}"

showBody (Body [] expr) = showExpr expr
showBody (Body statements expr) = showBlock $ map showStatement statements <> [showExpr expr]

showExpr (ELit s) = "\"" <> s <> "\""
showExpr (EApp id args) = id <> commaList (map showExpr args)
showExpr (ECat l r) = showExpr l <> " .. " <> showExpr r
showExpr (EMatch what branches) = "match " <> showExpr what <> " to " <> showBlock (map (\(pat, body) -> showPattern pat <> " -> " <> showBody body) branches)

showPattern (PLit s) = "\"" <>  s <> "\""
showPattern (PWild x) = x
showPattern (PCat l r) = showPattern l <> " .. " <> showPattern r

showStatement (SExpr e) = showExpr e
showStatement (SFunction id params body) = "let " <> id <> commaList (map showPattern params) <> showBody body
