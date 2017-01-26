module Language.Lambda.Parser where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

import Language.Lambda.Core

-- Runners

runExpressionParser :: String -> Expression
runExpressionParser input = case runParser expression "<input>" input of
  Left parseError -> error $ parseErrorPretty parseError
  Right e -> e

-- Lexing

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "#") (return ())

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

lambda :: Parser String
lambda = lexeme $ choice $ map string lambdaChars

arrow :: Parser String
arrow = lexeme $ choice $ map string arrowChars

-- Parsing

expression :: Parser Expression
expression = makeExprParser nonApp [[appOp]]
 where
  appOp = InfixL $ return Application

  nonApp :: Parser Expression
  nonApp = lexeme $ term <|> abstraction <|> (between (lexeme $ char '(') (lexeme $ char ')') nonApp)

  term :: Parser Expression
  term = Term <$> some alphaNumChar

  abstraction = do
    void lambda
    x <- lexeme $ some alphaNumChar
    void arrow
    body <- expression
    return (Abstraction x body)
