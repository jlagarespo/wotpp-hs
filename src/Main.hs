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
           <> progDesc "A dialect of the beloved wot++ programming language, written in Haskell."
           <> header "wot++ - A small macro language for producing and manipulating strings.")

  source <- IO.getContents
  let parseSource =
        case parse document "<stdin>" source of
          Left err -> Left $ ParseErr err
          Right x  -> Right x

  case parseSource >>= evalStatements newenv of
    Left err     -> print err
    Right (x, _) -> IO.putStrLn x
