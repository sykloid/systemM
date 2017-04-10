{-# LANGUAGE ApplicativeDo #-}

import Data.Semigroup
import Options.Applicative

import Language.Common.Pretty (pretty, render)
import qualified Language.Materialization.Parser as P
import Language.Materialization.Interpreter
import Language.Materialization.Transforms.AutoSync

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
  case P.runParser P.program (sourcePath options) source of
    Left e -> putStrLn $ P.parseErrorPretty e
    Right r -> do
      let result = runInterpretation (cfgToEnd $ autoSync r :/: nil) mempty
      putStrLn $ render $ pretty result
 where
  programOptionsParser = info (optionsParser <**> helper)
    ( fullDesc <> header "systemM -- A Materialization Calculus"
    )
