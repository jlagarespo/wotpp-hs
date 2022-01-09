module Error where

import Data.List (intercalate)
import Data.Text.Lazy (Text, unpack)
import Text.Parsec

import AST

data EvalError = NotInScope Identifier Int |
                 FunctionMatchFail Identifier Int Int |
                 MatchFail Expr Int
data Error = EvalErr EvalError Expr Backtrace | ParseErr ParseError

data TraceElement = TraceFunc Identifier [Expr] | TraceExpr Expr | TraceBranch [(Identifier, Text)] Pattern Body
type Backtrace = [TraceElement]

-- TODO: Use Text Show instead of String.
-- TODO: Position information for eval errors. (Add position information to AST nodes.)
-- TODO: Proper indentation.

showTrace :: Backtrace -> String
showTrace trace = if null trace then "" else "• " ++ intercalate "\n  " (map (\x -> "In " ++ show x) trace)

instance Show EvalError where
  show (NotInScope id args) = "Failed to appply " ++ show args ++ " arguments to function " ++ unpack id ++ ", which is not in scope."
  show (FunctionMatchFail id args count) = "Failed to match " ++ unpack id ++ " (" ++ show args ++ " arguments) against " ++ show count ++ " candidates."
  show (MatchFail what n) = "Failed to match " ++ show what ++ " against " ++ show n ++ " patterns. Please ensure all your matches are exhaustive."

instance Show Error where
  show (EvalErr err expr trace) = "eval error:\n• " ++ show err ++ "\n" ++ showTrace trace
  show (ParseErr err) = "parse error:\n• " ++ show err

instance Show TraceElement where
  show (TraceFunc id args) = "function call to " ++ unpack id ++ commaList (map show args)
  show (TraceExpr expr) = "expression:\n    " ++ show expr
  show (TraceBranch wildcards patt body) =
    "match branch:\n    " ++ show patt ++ " " ++ commaList (map (\(id, x) -> unpack id ++ " = " ++ unpack x) wildcards) ++
    " -> " ++ show body
