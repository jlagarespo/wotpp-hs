module Error where

import Text.Parsec

import AST

data EvalError = NotInScope Identifier Int
data Error = EvalErr EvalError | ParseErr ParseError

instance Show EvalError where
  show (NotInScope id args) = "Applying " ++ show args ++ " arguments to function " ++ id ++ " not in scope."

instance Show Error where
  show (EvalErr err)  = "eval: " ++ show err
  show (ParseErr err) = "parse: " ++show err
