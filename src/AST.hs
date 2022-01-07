module AST where

import Data.Text.Lazy (Text, unpack)
import Data.List (intercalate)

type Identifier = Text
data Body = Body [Statement] Expr
data Expr = ELit Text | EApp Identifier [Expr] | ECat Expr Expr | EMatch Expr [(Pattern, Body)]
data Pattern = PLit Text | PWild Identifier | PCat Pattern Pattern
data Statement = SExpr Expr | SFunction Identifier [Identifier] Body

commaList :: [String] -> String
commaList [] = ""
commaList x = "(" ++ intercalate ", " x ++ ")"

showBlock :: [String] -> String
showBlock statements = "{\n" ++ concatMap (\x -> "\t" ++ x ++ "\n") statements ++ "}"

instance Show Body where
  show (Body [] expr) = show expr
  show (Body statements expr) = showBlock $ map show statements ++ [show expr]

instance Show Expr where
  show (ELit s) = "\"" ++ unpack s ++ "\""
  show (EApp id args) = unpack id ++ commaList (map show args)
  show (ECat l r) = show l ++ " .. " ++ show r
  show (EMatch what branches) = "match " ++ show what ++ " to " ++ showBlock (map (\(pat, body) -> show pat ++ " -> " ++ show body) branches)

instance Show Pattern where
  show (PLit s) = "\"" ++ unpack s ++ "\""
  show (PWild x) = unpack x
  show (PCat l r) = show l ++ " .. " ++ show r

instance Show Statement where
  show (SExpr e) = show e
  show (SFunction id params body) =
    "let " ++ unpack id ++ commaList (map unpack params) ++ show body
