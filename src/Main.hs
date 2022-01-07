module Main where

import Text.Parsec

import Options.Applicative
import Data.Semigroup ((<>))

import Parser

main :: IO ()
main = do
  source <- getContents
  case parse document "<stdin>" source of
    Left err -> print err
    Right x  -> mapM_ print x
