module Main where

import Control.Monad.IO.Class
import Control.Monad.Except

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as IO

import Options.Applicative
import Data.Semigroup ((<>))

import AST
import Parser
import Error
import Eval

newtype Options = Options
  { dumpAST :: Bool }

options :: Parser Options
options = Options
  <$> switch (long "dump-ast" <> help "Dump the AST for debugging purposes.")

main :: IO ()
main = do
  opts <- execParser $
          info (options <**> helper)
          (fullDesc
           <> header "wot++ - A small macro language for producing and manipulating strings."
           <> progDesc "A dialect of the beloved wot++ programming language, written in Haskell.")

  source <- IO.getContents

  result <-
    runExceptT $
    do ast <- parseDocument source "<stdin>"
       if dumpAST opts
         then mapM_ (\stat -> liftIO $ IO.putStrLn (L.pack $ show stat) >>
                                       IO.putStrLn (showStatement stat)) ast
         else pure ()
       evalDocument ast

  case result of
    Left err -> IO.putStrLn $ showError err
    Right x  -> IO.putStrLn x
