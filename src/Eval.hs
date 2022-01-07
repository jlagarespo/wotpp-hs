module Eval where

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM

import Data.Either (rights)
import Data.List (find)
import Data.Maybe (fromMaybe)

import AST
import Error
import Util (onlyRights)

type FuncId = (Identifier, Int)
newtype Env = Env (HashMap (Identifier, Int) Function)
data Function = Function [Identifier] Body deriving Show

newenv :: Env
newenv = Env HM.empty

evalExpr :: Env -> Expr -> Either Error String
evalExpr _ (ELit str) = pure str
evalExpr (Env functions) (EApp id args) = do
  -- Retrieve function from environment.
  (Function params body) <-
    case functions !? (id, length args) of
      Nothing -> Left $ EvalErr $ NotInScope id (length args)
      Just f  -> pure f

  -- Add arguments to the environment as unevaluated functions.
  -- TODO: Parameter shadowing warnings.
  let newfuncs = HM.fromList $ zipWith (\param arg -> ((param, 0), Function [] (Body [] arg))) params args
      env' = Env (functions `HM.union` newfuncs)

  -- Evaluate all the statements to acquire our final environment.
  evalBody env' body

evalExpr env (ECat l r) = do
  l' <- evalExpr env l
  r' <- evalExpr env r
  pure $ l' ++ r'

evalExpr env (EMatch what branches) = do
  what' <- evalExpr env what
  branches' <- onlyRights $ map (\(l, r) -> (, r) <$> evalExpr env l) branches
  case find (\(l, r) -> l == what') branches' of
    Just (l, r) -> evalBody env r
    Nothing -> Left $ EvalErr $ MatchFail what (length branches)

evalBody :: Env -> Body -> Either Error String
evalBody env (Body statements expr) = do
  (_, newenv) <- evalStatements env statements
  evalExpr newenv expr


evalStatements :: Env -> [Statement] -> Either Error (String, Env)
evalStatements env ((SExpr expr):ss) = do
  (rest, env') <- evalStatements env ss
  e <- evalExpr env expr
  pure (e ++ rest, env')

evalStatements (Env functions) ((SFunction id params body):ss) =
  -- TODO: Function shadowing warnings.
  let func = Function params body
      newenv = Env (HM.insert (id, length params) func functions)
  in evalStatements newenv ss

evalStatements env [] = pure ("", env)
