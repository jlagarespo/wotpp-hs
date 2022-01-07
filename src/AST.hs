module AST where

import Data.Text.Lazy (Text, unpack)
import Data.List (intercalate)

type Identifier = Text
data Body = Body [Statement] Expr
data Expr = ELit Text | EApp Identifier [Expr] | ECat Expr Expr | EMatch Expr [(Expr, Body)]
data Statement = SExpr Expr | SFunction Identifier [Identifier] Body

showArglist :: [String] -> String
showArglist = intercalate ", "

instance Show Body where
  show (Body [] expr) = show expr ++ "\n"
  show (Body statements expr) =
    "{\n" ++ concatMap show statements ++ show expr ++ "\n}\n"

instance Show Expr where
  show (ELit s) = "\"" ++ unpack s ++ "\""
  show (EApp id args) = unpack id ++ "(" ++ showArglist (map show args) ++ ")"
  show (ECat l r) = show l ++ " .. " ++ show r
  show (EMatch what branches) = "match " ++ show what ++ " to " ++ concatMap (\(pat, body) -> show pat ++ " -> " ++ show body ++ " ") branches

instance Show Statement where
  show (SExpr e) = show e
  show (SFunction id params body) =
    "let " ++ unpack id ++ "(" ++ showArglist (map unpack params) ++ ") " ++ show body

-- TODO: Make the left side of each match branch a proper pattern, as opposed to an expression.
