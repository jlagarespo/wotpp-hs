module Lexer where

import Data.Char (isSpace)
import Text.Parsec (alphaNum, oneOf)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Token (LanguageDef, GenLanguageDef(..), makeTokenParser)
import qualified Data.Text.Lazy as L

import Data.Functor.Identity (Identity)

wotppDef :: GenLanguageDef L.Text u Identity
wotppDef = LanguageDef
           { commentStart    = "#["
           , commentEnd      = "]"
           , commentLine     = ""
           , nestedComments  = True
           , identStart      = identLetter wotppDef
           , identLetter     = alphaNum
           , opStart         = opLetter wotppDef
           , opLetter        = oneOf ".,->{}()"
           , reservedNames   = ["let", "match", "to"]
           , reservedOpNames = ["..", "->", ",", "{", "}", "(", ")"]
           , caseSensitive   = True }

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = makeTokenParser wotppDef

identifier = L.pack <$> Tok.identifier lexer
symbol s = Tok.symbol lexer (L.unpack s)
reserved s = Tok.reserved lexer (L.unpack s)
reservedOp s = Tok.reservedOp lexer (L.unpack s)
parens = Tok.parens lexer
-- TODO: Use proper wot++ like string lexing.
stringLiteral = L.pack <$> Tok.stringLiteral lexer
commaSep = Tok.commaSep lexer
