module Eval where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM

import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe, isJust)

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import AST
import Error
import Util (onlyRights, safeHead)

type FuncId = (Identifier, Int)
data Function = Function [Identifier] Body deriving Show
data Env = Env
  { functions :: HashMap FuncId Function
  , trace :: Backtrace }

evalError :: Env -> Expr -> EvalError -> Either Error a
evalError (Env _ trace) expr err = Left $ EvalErr err expr trace

newenv :: Env
newenv = Env HM.empty []

addFuncs :: [(FuncId, Function)] -> Env -> Env
addFuncs funcs env = env { functions = foldl (\fs (id, f) -> HM.insert id f fs) (functions env) funcs }

evalExpr :: Env -> Expr -> Either Error Text
evalExpr _ (ELit str) = pure str
evalExpr env e@(EApp id args) = do
  let fs = functions env

  -- Retrieve function from environment.
  (Function params body) <-
    case fs !? (id, length args) of
      Nothing -> evalError env e $ NotInScope id (length args)
      Just f  -> pure f

  -- Add arguments to the environment as unevaluated functions.
  -- TODO: Parameter shadowing warnings.
  let newfuncs = zipWith (\param arg -> ((param, 0), Function [] (Body [] arg))) params args
      env' = addFuncs newfuncs env { trace = TraceFunc id args:trace env }

  -- Evaluate all the statements to acquire our final environment.
  evalBody env' body

evalExpr env (ECat l r) = do
  -- Concat.
  l' <- evalExpr env l
  r' <- evalExpr env r
  pure $ l' <> r'

evalExpr env e@(EMatch what branches) = do
  -- Evaluate the string we are going to be matching against.
  str <- evalExpr env what
  -- Attempt to match that string against every branch.
  let branches' = mapMaybe (\(patt, body) -> do wildcards <- fitPattern str patt
                                                pure (wildcards, patt, body)) branches
  -- We take the first succesful match and evaluate the right-hand side. If none match, error.
  case safeHead branches' of
    Just (wildcards, patt, body) ->
      let newfuncs = map (\(id, val) -> ((id, 0), Function [] (Body [] (ELit val)))) wildcards
      in evalBody (addFuncs newfuncs env { trace = TraceBranch wildcards patt body:trace env }) body
    Nothing -> evalError env e $ MatchFail what (length branches)

  where
    -- The actual pattern matching. Fairly inefficient.
    fitPattern :: Text -> Pattern -> Maybe [(Identifier, Text)]
    fitPattern input (PLit x) =
      if input == x then Just [] else Nothing
    fitPattern input (PWild id) = Just [(id, input)]
    fitPattern input (PCat l r) =
      let candidates =
            mapMaybe (\(init, tail) -> do l' <- fitPattern init l
                                          r' <- fitPattern tail r
                                          pure $ l' ++ r')
            (zip (L.inits input) (L.tails input)) in safeHead candidates

evalBody :: Env -> Body -> Either Error Text
evalBody env (Body statements expr) = do
  (_, env') <- evalStatements env statements
  evalExpr env' expr

evalStatements :: Env -> [Statement] -> Either Error (Text, Env)
evalStatements env ((SExpr expr):ss) = do
  (rest, env') <- evalStatements env ss
  e <- evalExpr (env { trace = TraceExpr expr:trace env }) expr
  pure (e <> rest, env')

evalStatements env ((SFunction id params body):ss) =
  -- TODO: Function shadowing warnings (or errors?).
  let func = Function params body
      env' = addFuncs [((id, length params), func)] env
  in evalStatements env' ss

evalStatements env [] = pure ("", env)
