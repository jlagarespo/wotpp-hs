module Eval where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM

import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe, isJust)

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import AST
import Error
import Util (onlyRights, safeHead)

data Function = Function [Identifier] Body deriving Show
data Env = Env
  { functions :: HashMap Identifier [Function]
  , trace :: Backtrace }

evalError :: Env -> Expr -> EvalError -> Either Error a
evalError (Env _ trace) expr err = Left $ EvalErr err expr trace

newenv :: Env
newenv = Env HM.empty []

addConstants :: [(Identifier, Expr)] -> Env -> Env
addConstants ks env =
  env { functions = foldl (\fs (id, k) -> HM.insert id [Function [] (Body [] k)] fs)
                    (functions env) ks }

evalExpr :: Env -> Expr -> Either Error Text
evalExpr _ (ELit str) = pure str
evalExpr env e@(EApp id args) = do
  -- Retrieve all functions with identifier 'id' from the environment.
  candidates <-
    case functions env !? id of
      Nothing -> evalError env e $ NotInScope id (length args)
      Just fs -> pure fs

  -- Match the provided arguments against them.
  (Function params body) <-
    case find (\(Function params _) -> length params == length args) candidates of
      Nothing -> evalError env e $ FunctionMatchFail id (length args) (length candidates)
      Just f  -> pure f

  -- Add arguments to the environment as parameterless functions (constants.)
  -- TODO: Parameter shadowing warnings.
  -- Arguments MUST be strictly evaluated, since if they're lazily evaluated, the environment
  -- might've changed a few levels deep. TODO: Store the environment along with the arguments to
  -- evaluate them later (I suppose a sort of thunk?)
  argValues <- mapM (fmap ELit . evalExpr env) args
  let env' = addConstants (zip params argValues) env { trace = TraceFunc id args:trace env }

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
      let env' = addConstants (map (second ELit) wildcards) env
                 { trace = TraceBranch wildcards patt body:trace env }
      in evalBody env' body
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
  e <- evalExpr (env { trace = TraceExpr expr:trace env }) expr
  (rest, env') <- evalStatements env ss
  pure (e <> rest, env')

evalStatements env ((SFunction id params body):ss) =
  -- TODO: Function shadowing warnings (or errors?).
  let func = Function params body
      env' = env { functions = HM.alter (\case Just fs -> Just $ func:fs
                                               Nothing -> Just [func])
                               id (functions env) }
  in evalStatements env' ss

evalStatements env [] = pure ("", env)
