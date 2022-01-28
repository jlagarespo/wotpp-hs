module Main where

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
  let document = parseDocument source "<stdin>"

  case document of
    Left err -> IO.putStrLn $ showError err
    Right ast -> do
      if dumpAST opts
        then mapM_ (\stat -> IO.putStrLn (L.pack $ show stat) >> IO.putStrLn (showStatement stat)) ast
        else pure ()

      case evalDocument ast of
        Left err -> IO.putStrLn $ showError err
        Right x  -> IO.putStrLn x
