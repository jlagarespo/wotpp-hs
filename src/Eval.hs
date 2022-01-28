module Eval where

import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import Control.Monad.Except

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as IO
import qualified Data.Text.Lazy as L

import Data.HashMap.Strict (HashMap, empty, (!?))
import qualified Data.HashMap.Strict as HM

import Data.Bifunctor (second)
import Data.List (find)
import Data.Maybe (mapMaybe, catMaybes, isJust, isNothing, fromJust, listToMaybe)
import Data.Hashable (Hashable)

import Text.Parsec.Text.Lazy (Parser)

import AST
import Error
import Parser
import Util (firstOne, unimplemented)

data Function = Function [Pattern] Body
data Env = Env
  { functions :: HashMap Identifier [Function]
  , arguments :: HashMap Identifier Expr
  , trace :: Backtrace }

type Eval = StateT Env (ExceptT Error IO)

newenv :: Env
newenv = Env empty empty []

insertMany :: (Eq k, Hashable k) => HashMap k v -> [(k, v)] -> HashMap k v
insertMany = foldl (\hm (k, v) -> HM.insert k v hm)

evalError :: Expr -> EvalError -> Eval a
evalError expr err = do
  (Env _ _ trace) <- get
  lift $ throwError $ EvalErr err expr trace

evalPure :: Env -> Eval a -> Eval a
evalPure env x = do
  env' <- get
  put env
  result <- x
  put env'
  pure result

-- |Evaluate an expression.
evalExpr :: Expr -> Eval Text
evalExpr (ELit str) = pure str
evalExpr e@(EApp id args) = do
  -- Evaluate args. (sadly, since we need to pattern match them against function candidates, we need
  -- to know their value, hence why we cannot build a thunk and lazily evaluate them.) That is
  -- except for parameters that are just a wildcard. TODO: Look for only-wildcard parameters and
  -- lazily evaluate them.
  argValues <- mapM evalExpr args
  env <- get

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
            Nothing -> evalError e $ NotInScope id (length args)
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
          Nothing -> evalError e $ FunctionMatchFail id (length args) (length candidates)
          Just r  -> pure r

      -- Otherwise, it must be an argument.
      Just arg -> pure (Function [] $ Body [] arg, [])

  -- Add arguments to the environment as parameterless functions (constants.)
  let newenv = env { arguments = insertMany (arguments env) (map (second ELit) wildcards)
                   , trace = TraceFunc id args:trace env }

  -- Evaluate all the statements to acquire our final environment.
  evalPure newenv $ evalBody body

evalExpr e@(EBuiltin name args) = do
  argValues <- mapM evalExpr args
  case builtins !? name of
    Just builtin -> builtin argValues
    Nothing -> evalError e $ NoBuiltin name

  where
    builtins :: HashMap Text ([Text] -> Eval Text)
    builtins = HM.fromList
      [ ("eval", \args ->
            do statements <- lift $ parseDocument (L.concat args) "<eval>"
               L.concat <$> mapM evalStatement statements) ]

evalExpr (ECat l r) = do
  -- Concat.
  l' <- evalExpr l
  r' <- evalExpr r
  pure $ l' <> r'

evalExpr e@(EMatch what branches) = do
  -- Evaluate the string we are going to be matching against.
  str <- evalExpr what

  -- Attempt to match that string against every branch.
  let chosen = firstOne $ map (\(patt, body) -> do wildcards <- fitPattern str patt
                                                   pure (wildcards, patt, body)) branches

  -- We take the first succesful match and evaluate the right-hand side. If none match, error.
  case chosen of
    Just (wildcards, patt, body) -> do
      env <- get
      let newenv = env { arguments = insertMany (arguments env) (map (second ELit) wildcards)
                       , trace = TraceBranch wildcards patt body:trace env }
      evalPure newenv $ evalBody body
    Nothing -> evalError e $ MatchFail what (length branches)

-- |Fairly inefficient string pattern matching algorithm.
fitPattern :: Text -> Pattern -> Maybe [(Identifier, Text)]
fitPattern input (PLit x) =
  if input == x then Just [] else Nothing
fitPattern input (PWild id) = Just [(id, input)]
fitPattern input (PCat l@(PLit lit) r) = do
  l' <- fitPattern (L.take (L.length lit) input) l
  r' <- fitPattern (L.drop (L.length lit) input) r
  pure $ l' ++ r'
fitPattern input (PCat l r) =
  let candidates =
        mapMaybe (\(init, tail) -> do l' <- fitPattern init l
                                      r' <- fitPattern tail r
                                      pure $ l' ++ r')
        (zip (L.inits input) (L.tails input)) in listToMaybe candidates

-- |Evaluate every statement of a body, and use the resulting environment to evaluate and return the
-- trailing expression.
evalBody :: Body -> Eval Text
evalBody (Body statements expr) = do
  mapM_ evalStatement statements
  evalExpr expr

-- |Evaluate a statement.
evalStatement :: Statement -> Eval Text
evalStatement (SExpr expr) = do
  env <- get
  put $ env { trace = TraceExpr expr:trace env }
  evalExpr expr

evalStatement (SFunction id params body) = do
  -- TODO: Function shadowing warnings (or errors?).
  env <- get
  let func = Function params body
      env' = env { functions = HM.alter (\case Just fs -> Just $ fs ++ [func]
                                               Nothing -> Just [func])
                               id (functions env) }
  put env'
  pure ""

evalStatement (SInclude fileExpr) = do
  filename <- evalExpr fileExpr
  source <- liftIO $ IO.readFile (L.unpack filename)
  statements <- lift $ parseDocument source filename
  -- L.concat <$> mapM evalStatement statements
  mapM_ evalStatement statements
  pure ""

-- |Evaluate a list of statements, and concat their results.
evalDocument :: [Statement] -> ExceptT Error IO Text
evalDocument statements = evalStateT (L.concat <$> mapM evalStatement statements) newenv
