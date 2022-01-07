module Eval where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM

import Data.Either (rights)
import Data.List (find)
import Data.Maybe (fromMaybe)

import AST
import Error
import Util (onlyRights)

type FuncId = (Identifier, Int)
data Function = Function [Identifier] Body deriving Show
data Env = Env
  { functions :: HashMap FuncId Function
  , trace :: [Expr] }

evalError :: Env -> EvalError -> Either Error a
evalError (Env _ trace) err = Left $ EvalErr err trace

newenv :: Env
newenv = Env HM.empty []

evalExpr :: Env -> Expr -> Either Error Text
evalExpr _ (ELit str) = pure str
evalExpr (Env functions trace) e@(EApp id args) = do
  let env = Env functions (e:trace)

  -- Retrieve function from environment.
  (Function params body) <-
    case functions !? (id, length args) of
      Nothing -> evalError env $ NotInScope id (length args)
      Just f  -> pure f

  -- Add arguments to the environment as unevaluated functions.
  -- TODO: Parameter shadowing warnings.
  let newfuncs = HM.fromList $ zipWith (\param arg -> ((param, 0), Function [] (Body [] arg))) params args
      env' = Env (functions `HM.union` newfuncs) (e:trace)

  -- Evaluate all the statements to acquire our final environment.
  evalBody env' body

evalExpr (Env fs trace) e@(ECat l r) = do
  let env = Env fs (e:trace)
  l' <- evalExpr env l
  r' <- evalExpr env r
  pure $ l' <> r'

evalExpr (Env fs trace) e@(EMatch what branches) = do
  let env = Env fs (e:trace)
  what' <- evalExpr env what
  branches' <- onlyRights $ map (\(l, r) -> (, r) <$> evalExpr env l) branches
  case find (\(l, r) -> l == what') branches' of
    Just (l, r) -> evalBody env r
    Nothing -> evalError env $ MatchFail what (length branches)

evalBody :: Env -> Body -> Either Error Text
evalBody env (Body statements expr) = do
  (_, newenv) <- evalStatements env statements
  evalExpr newenv expr


evalStatements :: Env -> [Statement] -> Either Error (Text, Env)
evalStatements env@(Env fs _) ((SExpr expr):ss) = do
  (rest, env') <- evalStatements env ss
  e <- evalExpr (Env fs []) expr
  pure (e <> rest, env')

evalStatements (Env functions _) ((SFunction id params body):ss) =
  -- TODO: Function shadowing warnings.
  let func = Function params body
      newenv = Env (HM.insert (id, length params) func functions) []
  in evalStatements newenv ss

evalStatements env [] = pure ("", env)
