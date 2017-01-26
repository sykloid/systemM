module Language.Materialization.Parser where

import Control.Monad (void)

import Text.Megaparsec

import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec.String

import Language.Materialization.Core

-- Runners

runProgramParser :: String -> Program
runProgramParser input = case runParser program "<input>" input of
  Left parseError -> error $ parseErrorPretty parseError
  Right p -> p

-- Lexing

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "#") (return ())

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

lambda :: Parser Char
lambda = lexeme $ oneOf "\\"

comma :: Parser Char
comma = lexeme $ char ','

semi :: Parser Char
semi = lexeme $ char ';'

-- Parsing

program :: Parser Program
program = do
  body <- block
  void $ lexeme (string "~>")
  returnLoc <- location
  return (Program body returnLoc)

block :: Parser Block
block = Block <$> between (lexeme $ char '{') (lexeme $ char '}') (sepEndBy clause semi)

clause :: Parser Clause
clause = (allocate <|> try initialize <|> try materialize <|> deallocate)
 where
  allocate = (lexeme $ char '+') >> Allocate <$> location
  initialize = do
    loc <- location
    void $ lexeme (char '=')
    vexpr <- valueExpression
    return (Initialize loc vexpr)
  materialize = do
    loc <- location
    m <- mgets
    lexpr <- locationExpression
    return (Materialize loc lexpr m)
  deallocate = (lexeme $ char '-') >> Allocate <$> location

valueExpression :: Parser ValueExpression
valueExpression = vPrimitive <|> abstraction
 where
  vPrimitive = VPrimitive <$> value
  abstraction = do
    void $ lexeme lambda
    fLoc <- location
    cSpec <- closureSpec
    body <- program
    return (Abstraction fLoc cSpec body)

locationExpression :: Parser LocationExpression
locationExpression = lPrimitive <|> application
 where
   lPrimitive = LPrimitive <$> location
   application = do
     fLoc <- location
     m <- mapply
     xLoc <- location
     return $ Application fLoc xLoc m

closureSpec :: Parser CaptureSpec
closureSpec = CaptureSpec <$> between (lexeme $ char '[') (lexeme $ char ']') (sepEndBy captureSpec comma)
 where
  captureSpec :: Parser (Location, Location, Method)
  captureSpec = do
    iLoc <- location
    m <- mgets
    oLoc <- location
    return (iLoc, oLoc, m)

method :: Parser Method
method = oneOf "CMR" >>= \m -> return $ case m of
  'C' -> Copy
  'M' -> Move
  'R' -> Refr
  _ -> error "unreachable"

location :: Parser Location
location = Location <$> lexeme (some alphaNumChar)

value :: Parser Value
value = atom <|> closure
 where
  atom = string "atom" *> pure Atom
  closure = do
    void lambda
    xLoc <- location
    cLocs <- between (lexeme $ char '[') (lexeme $ char ']') (sepEndBy location comma)
    body <- program
    return (Closure xLoc (ClosureSpec cLocs) body)

-- Helpers

mgets :: Parser Method
mgets = lexeme (char '<' *> method <* char '-')

mapply :: Parser Method
mapply = lexeme (between (char '(') (char ')') method)
