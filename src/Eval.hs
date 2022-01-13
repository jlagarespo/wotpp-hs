module Eval where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L

import Data.HashMap.Strict (HashMap, empty, (!?))
import qualified Data.HashMap.Strict as HM

import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (mapMaybe, catMaybes, isJust, isNothing, fromJust, listToMaybe)
import Data.Hashable

import Text.Parsec.Text.Lazy (Parser)

import AST
import Error
import Util (firstOne, unimplemented)

data Function = Function [Pattern] Body
data Env = Env
  { functions :: HashMap Identifier [Function]
  , arguments :: HashMap Identifier Expr
  , trace :: Backtrace }

evalError :: Env -> Expr -> EvalError -> Either Error a
evalError (Env _ _ trace) expr err = Left $ EvalErr err expr trace

newenv :: Env
newenv = Env empty empty []

insertMany :: (Eq k, Hashable k) => HashMap k v -> [(k, v)] -> HashMap k v
insertMany = foldl (\hm (k, v) -> HM.insert k v hm)

-- |Evaluate an expression.
evalExpr :: Env -> Expr -> Either Error Text
evalExpr _ (ELit str) = pure str
evalExpr env e@(EApp id args) = do
  -- Evaluate args. (sadly, since we need to pattern match them against function candidates, we need
  -- to know their value, hence why we cannot build a thunk and lazily evaluate them.) That is
  -- except for parameters that are just a wildcard. TODO: Look for only-wildcard parameters and
  -- lazily evaluate them.
  argValues <- mapM (evalExpr env) args

  -- Retrieve function from environment.
  (Function params body, wildcards) <-
    -- Check whether the identifier we're calling refers to a function or an argument (arguments are
    -- scope-local and shadow functions.)
    case arguments env !? id of
      -- If it's not, look for a proper function.
      Nothing -> do
        -- Retrieve all functions with identifier 'id' from the environment.
        candidates <-
          case functions env !? id of
            Nothing -> evalError env e $ NotInScope id (length args)
            Just fs -> pure fs

        -- Try to match the provided arguments against their respective patterns (the function's
        -- params), and return this first one that succeeds. Error if none does.
        let chosen = firstOne $
                     map (\f@(Function params _) ->
                            let wildcards = zipWith fitPattern argValues params
                            in if length argValues /= length params || any isNothing wildcards
                               then Nothing
                               else Just (f, concat $ catMaybes wildcards)) candidates

        -- Take the first candidate that matches and return it. If none do, throw an error.
        case chosen of
          Nothing -> evalError env e $ FunctionMatchFail id (length args) (length candidates)
          Just r  -> pure r

      -- Otherwise, it must be an argument.
      Just arg -> pure (Function [] $ Body [] arg, [])

  -- Add arguments to the environment as parameterless functions (constants.)
  let env' = env { arguments = insertMany (arguments env) (map (second ELit) wildcards)
                 , trace = TraceFunc id args:trace env }

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
  let chosen = firstOne $ map (\(patt, body) -> do wildcards <- fitPattern str patt
                                                   pure (wildcards, patt, body)) branches

  -- We take the first succesful match and evaluate the right-hand side. If none match, error.
  case chosen of
    Just (wildcards, patt, body) ->
      let env' = env { arguments = insertMany (arguments env) (map (second ELit) wildcards)
                     , trace = TraceBranch wildcards patt body:trace env }
      in evalBody env' body
    Nothing -> evalError env e $ MatchFail what (length branches)

-- |Fairly inefficient string pattern matching algorithm.
fitPattern :: Text -> Pattern -> Maybe [(Identifier, Text)]
fitPattern input (PLit x) =
  if input == x then Just [] else Nothing
fitPattern input (PWild id) = Just [(id, input)]
fitPattern input (PCat l r) =
  let candidates =
        mapMaybe (\(init, tail) -> do l' <- fitPattern init l
                                      r' <- fitPattern tail r
                                      pure $ l' ++ r')
        (zip (L.inits input) (L.tails input)) in listToMaybe candidates

-- |Evaluate every statement of a body, and use the generate environment to evaluate and return the
-- trailing expression.
evalBody :: Env -> Body -> Either Error Text
evalBody env (Body statements expr) = do
  env' <- foldl (\e statement -> do
                    env <- e
                    (_, env') <- evalStatement env statement
                    pure env') (pure env) statements
  evalExpr env' expr

-- |Evaluate a statement.
evalStatement :: Env -> Statement -> Either Error (Text, Env)
evalStatement env (SExpr expr) = do
  str <- evalExpr (env { trace = TraceExpr expr:trace env }) expr
  pure (str, env)

evalStatement env (SFunction id params body) =
  -- TODO: Function shadowing warnings (or errors?).
  let func = Function params body
      env' = env { functions = HM.alter (\case Just fs -> Just $ fs ++ [func]
                                               Nothing -> Just [func])
                               id (functions env) }
  in pure ("", env')

-- |Evaluate a list of statements, and concat their results.
evalDocument :: [Statement] -> Either Error Text
evalDocument statements =
  fst <$> foldl (\acc statement -> do
                    (str, env) <- acc
                    (str', env') <- evalStatement env statement
                    pure (str <> str', env')) (pure ("", newenv)) statements
