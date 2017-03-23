module Language.Materialization.Parser where

import Control.Monad (void)

import Text.Megaparsec

import Text.Megaparsec.String

import Language.Common.Parser
import Language.Materialization.Core

-- Runners

runProgramParser :: String -> Program
runProgramParser input = case runParser program "<input>" input of
  Left parseError -> error $ parseErrorPretty parseError
  Right p -> p

-- Parsing

program :: Parser Program
program = sepEndBy clause semi

clause :: Parser Clause
clause = try pAssignment <|> try pSynchronization <|> try pReturn
 where
  pAssignment = do
    lExpr <- leftExpression
    void (lexeme $ char '=')
    rExpr <- rightExpression
    return (Assignment lExpr rExpr)
  pSynchronization = do
    lExpr <- leftExpression
    void $ lexeme (char '?')
    return (Synchronization lExpr)
  pReturn = lexeme (char '!') >> return Return

leftExpression :: Parser LeftExpression
leftExpression = sepBy1 name (lexeme $ char '.') >>= \(uqn:qns) ->
  return $ if null qns then Unqualified uqn else foldl Qualified (Unqualified uqn) qns

rightExpression :: Parser RightExpression
rightExpression = bidExpression <|> literalExpression <|> application
 where
  bidExpression = BidExpression <$> bid <?> "Bid"
  application = Application <$> leftExpression <*> bid
  literalExpression = LiteralExpression <$> literal

literal :: Parser Literal
literal = smallLiteral <|> largeLiteral <|> captureExpression
 where
  smallLiteral = lexeme (string "small") *> pure SmallLiteral
  largeLiteral = lexeme (string "large") *> pure LargeLiteral
  captureExpression = do
    void $ lexeme (char '\\')
    cSpec <- captureSpec
    void $ optional (lexeme $ char '.')
    f <- abstraction
    return (CaptureExpression cSpec f)

abstraction :: Parser Abstraction
abstraction = do
  void $ lexeme (char '\\')
  formalArg <- name
  void $ optional (lexeme $ char '.')
  body <- between (lexeme $ char '{') (lexeme $ char '}') program
  void $ lexeme (string "->")
  rt <- rightExpression
  return (Abstraction formalArg body rt)

bid :: Parser Bid
bid = between (lexeme $ char '(') (lexeme $ char ')') $ do
  lExpr <- leftExpression
  void (lexeme $ char '*')
  bt <- bidType
  return (Bid lExpr bt)

captureSpec :: Parser CaptureSpec
captureSpec = between (lexeme $ char '[') (lexeme $ char ']') (sepEndBy captureSpec1 comma)
 where
  captureSpec1 :: Parser (Name, Bid)
  captureSpec1 = do
    iName <- name
    void (lexeme $ char '=')
    b <- bid
    return (iName, b)

bidType :: Parser BidType
bidType = oneOf "CMR" >>= \m -> return $ case m of
  'C' -> Copy
  'M' -> Move
  'R' -> Refr
  _ -> error "unreachable"

name :: Parser Name
name = Name <$> lexeme (some alphaNumChar)
