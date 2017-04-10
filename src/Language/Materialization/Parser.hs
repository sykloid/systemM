module Language.Materialization.Parser (
  Parser,
  runParser,
  forceParser,
  parseErrorPretty,

  -- * Parsers
  program,
  block,
  clause,

  leftExpression,
  syncExpression,
  rightExpression,
  literal,
  abstraction,
  bid,
  captureSpec,
  bidType,
  name
) where

import Control.Monad (void)

import Text.Megaparsec

import Text.Megaparsec.String

import Language.Common.Parser
import Language.Materialization.Core

-- Runners

forceParser :: Parser a -> String -> a
forceParser p = either (error . parseErrorPretty) id . runParser p "<input>"

-- Parsing

program :: Parser Program
program = space *> block <* eof

block :: Parser Program
block = sepEndBy clause semi

clause :: Parser Clause
clause = try pAssignment <|> pSynchronization <|> pReturn
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

syncExpression :: Parser SyncExpression
syncExpression = do
  lExpr <- leftExpression
  sync <- optional (lexeme $ char '?')
  return $ case sync of
    Nothing -> NonSynchronizing lExpr
    Just _ -> Synchronizing lExpr

rightExpression :: Parser RightExpression
rightExpression = try bidExpression <|> try literalExpression <|> application
 where
  bidExpression = BidExpression <$> bid <?> "bid"
  application = Application <$> syncExpression <*> bid
  literalExpression = LiteralExpression <$> literal

literal :: Parser Literal
literal = smallLiteral <|> largeLiteral <|> captureExpression
 where
  smallLiteral = SmallLiteral <$> (lexeme (string "small") *> lexeme (char '-') *> valueSentinel)
  largeLiteral = LargeLiteral <$> (lexeme (string "large") *> lexeme (char '-') *> valueSentinel)
  valueSentinel = ValueSentinel <$> some alphaNumChar
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
  body <- between (lexeme $ char '{') (lexeme $ char '}') block
  void $ lexeme (string "->")
  rt <- rightExpression
  return (Abstraction formalArg body rt)

bid :: Parser Bid
bid = between (lexeme $ char '(') (lexeme $ char ')') $ do
  sExpr <- syncExpression
  void (lexeme $ char '*')
  bt <- bidType
  return (Bid sExpr bt)

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
bidType = char 'C' <|> char 'M' <|> char 'R' >>= \m -> return $ case m of
  'C' -> Copy
  'M' -> Move
  'R' -> Refr
  _ -> error "unreachable"

name :: Parser Name
name = Name <$> lexeme (some alphaNumChar)
