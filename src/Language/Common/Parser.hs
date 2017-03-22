module Language.Common.Parser where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

import Language.Common

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lambda :: Parser String
lambda = lexeme $ choice $ map string lambdaChars

arrow :: Parser String
arrow = lexeme $ choice $ map string arrowChars

comma :: Parser Char
comma = lexeme $ char ','

semi :: Parser Char
semi = lexeme $ char ';'
