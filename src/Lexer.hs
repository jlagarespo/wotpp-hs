module Lexer where

import Data.Char (isSpace)
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Token (LanguageDef, GenLanguageDef(..), makeTokenParser)

wotppDef :: LanguageDef st
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

lexer = makeTokenParser wotppDef

identifier = Tok.identifier lexer
symbol = Tok.symbol lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
-- TODO: Use proper wot++ like string lexing.
stringLiteral = Tok.stringLiteral lexer
