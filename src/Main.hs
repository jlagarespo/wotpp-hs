module Main where

import Data.Text.Lazy.IO as IO
import Text.Parsec

import Options.Applicative
import Data.Semigroup ((<>))

import Parser
import Error
import Eval

data Options = Options

options :: Parser Options
options = pure Options

main :: IO ()
main = do
  opts <- execParser $
          info (options <**> helper)
          (fullDesc
           <> header "wot++ - A small macro language for producing and manipulating strings."
           <> progDesc "A dialect of the beloved wot++ programming language, written in Haskell.")

  source <- IO.getContents
  case parseDocument source >>= evalDocument newenv of
    Left err -> IO.putStrLn $ showError err
    Right x  -> IO.putStrLn x
