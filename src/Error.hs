module Error where

import Data.List (intercalate)
import Data.Text.Lazy (Text, unpack)
import Text.Parsec

import AST

data EvalError = NotInScope Identifier Int | MatchFail Expr Int
data Error = EvalErr EvalError [Expr] | ParseErr ParseError

instance Show EvalError where
  show (NotInScope id args) = "Applying " ++ show args ++ " arguments to function " ++ unpack id ++ " not in scope."
  show (MatchFail what n) = "Attempted to match " ++ show what ++ " against " ++ show n ++ " patterns. Please ensure all your matches are exhaustive."

instance Show Error where
  show (EvalErr err trace) =
    (if null trace
     then "eval error:\n"
     else "while evaluating " ++ intercalate "\n in " (map show trace) ++ ":\n")
    ++ show err
  show (ParseErr err) = "parse error:\n" ++show err
