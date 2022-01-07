module Error where

import Text.Parsec

import AST

data EvalError = NotInScope Identifier Int | MatchFail Expr Int
data Error = EvalErr EvalError | ParseErr ParseError

instance Show EvalError where
  show (NotInScope id args) = "Applying " ++ show args ++ " arguments to function " ++ id ++ " not in scope."
  show (MatchFail what n) = "Attempted to match " ++ show what ++ " against " ++ show n ++ " patterns. Please ensure all your matches are exhaustive."

instance Show Error where
  show (EvalErr err)  = "eval: " ++ show err
  show (ParseErr err) = "parse: " ++show err
