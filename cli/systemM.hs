{-# LANGUAGE ApplicativeDo #-}

import Data.Semigroup
import Options.Applicative

import Language.Common.Pretty (pretty, render)
import Language.Materialization.Parser
import Language.Materialization.Interpreter

newtype Options = Options
  { sourcePath :: String
  }

optionsParser :: Parser Options
optionsParser = do
  src <- argument str (metavar "PATH")
  pure Options { sourcePath = src }

main :: IO ()
main = do
  options <- execParser programOptionsParser
  source <- readFile (sourcePath options)
  case runProgramParser source of
    Left e -> putStrLn $ parseErrorPretty e
    Right r -> do
      let result = runInterpretation (cfgToEnd $ r :/: nil) mempty
      putStrLn $ render $ pretty result
 where
  programOptionsParser = info (optionsParser <**> helper)
    ( fullDesc <> header "systemM -- A Materialization Calculus"
    )
