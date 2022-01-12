module Error where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import TextShow
import Text.Parsec

import AST

data EvalError = NotInScope Identifier Int |
                 FunctionMatchFail Identifier Int Int |
                 MatchFail Expr Int
data Error = EvalErr EvalError Expr Backtrace | ParseErr ParseError

data TraceElement = TraceFunc Identifier [Expr] | TraceExpr Expr | TraceBranch [(Identifier, Text)] Pattern Body
type Backtrace = [TraceElement]

-- TODO: Position information for eval errors. (Add position information to AST nodes.)
-- TODO: Proper indentation.

showTrace :: Backtrace -> Text
showTrace trace = if null trace then "" else "• " <> L.intercalate "\n  " (map (\x -> "In " <> showTraceElement x) trace)

showTraceElement :: TraceElement -> Text
showTraceElement (TraceFunc id args) = "function call to " <> id <> commaList (map showExpr args)
showTraceElement (TraceExpr expr) = "expression:\n    " <> showExpr expr
showTraceElement (TraceBranch wildcards patt body) =
  "match branch:\n    " <> showPattern patt <> " " <> commaList (map (\(id, x) -> id <> " = " <> x) wildcards) <>
  " -> " <> showBody body

showEvalError :: EvalError -> Text
showEvalError (NotInScope id args) = "Failed to appply " <> showtl args <> " arguments to function " <> id <> ", which is not in scope."
showEvalError (FunctionMatchFail id args count) = "Failed to match " <> id <> " (" <> showtl args <> " arguments) against " <> showtl count <> " candidates."
showEvalError (MatchFail what n) = "Failed to match " <> showExpr what <> " against " <> showtl n <> " patterns. Please ensure all your matches are exhaustive."

showError :: Error -> Text
showError (EvalErr err expr trace) = "eval error:\n• " <> showEvalError err <> "\n" <> showTrace trace
showError (ParseErr err) = "parse error:\n• " <> L.pack (show err)
