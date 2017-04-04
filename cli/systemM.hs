{-# LANGUAGE ApplicativeDo #-}

import Data.Semigroup
import Options.Applicative

import Language.Common.Pretty (pretty, render)
import Language.Materialization.Parser

data Options = Options
  { sourcePath :: String
  }

optionsParser :: Parser Options
optionsParser = Options <$> argument str (metavar "PATH")

main :: IO ()
main = do
  options <- execParser programOptionsParser
  source <- readFile (sourcePath options)
  case runProgramParser source of
    Left e -> putStrLn $ parseErrorPretty e
    Right r -> putStrLn $ render $ pretty r
 where
  programOptionsParser = info (optionsParser <**> helper)
    ( fullDesc <> header "systemM -- A Materialization Calculus"
    )
