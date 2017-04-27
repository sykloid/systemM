{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Semigroup
import Options.Applicative

import Language.Common.Pretty (pretty, renderPretty)
import qualified Language.Lambda.Core as LC
import qualified Language.Lambda.Parser as LP
import qualified Language.Materialization.Core as MC
import qualified Language.Materialization.Parser as P
import Language.Materialization.Interpreter
import Language.Materialization.Interpreter.Diagrams
import Language.Materialization.Transforms.AutoSync

newtype Options = Options
  { mode :: Mode
  }

data Mode
  = InterpretSystemM { diagramPath :: Maybe FilePath, sourcePath :: FilePath }
  | TranslateLambda { sourcePath :: FilePath }
  | ParseLambda { sourcePath :: FilePath }

optionsParser :: Parser Options
optionsParser = do
  mode <- subparser ( command "interpret" (info interpretSystemM idm)
                   <> command "interpret-translate-lambda" (info translateLambda idm)
                   <> command "parse-lambda" (info parseLambda idm)
                    )
  pure Options { mode = mode }

interpretSystemM :: Parser Mode
interpretSystemM = do
  sourcePath' <- argument str (metavar "PATH")
  diagramPath' <- optional $ strOption
    ( long "diagram-path"
   <> short 'D'
   <> help "Path to put diagrams in."
   <> metavar "PATH"
    )
  pure $ InterpretSystemM diagramPath' sourcePath'

translateLambda :: Parser Mode
translateLambda = TranslateLambda <$> argument str (metavar "PATH")

parseLambda :: Parser Mode
parseLambda = ParseLambda <$> argument str (metavar "PATH")

dispatchForMode :: Options -> IO ()
dispatchForMode opt = case mode opt of
  InterpretSystemM {..} -> do
    source <- readFile sourcePath
    case P.runParser P.program sourcePath source of
        Left e -> putStrLn $ P.parseErrorPretty e
        Right r -> do
          let result = runInterpretation (cfgToEnd $ autoSync r :/: nil) mempty
          putStrLn $ renderPretty $ pretty result
          case diagramPath of
            Nothing -> return ()
            Just dP -> visualizeResult result dP
  TranslateLambda {..} -> do
    source <- readFile sourcePath
    case P.runParser LP.expression sourcePath source of
      Left parseError -> putStrLn $ P.parseErrorPretty parseError
      Right e -> let (p, lExpr) = LC.toSystemM e
                     p' = p ++ [MC.Synchronization lExpr]
                     result = runInterpretation (cfgToEnd $ autoSync p' :/: nil) mempty
                 in putStrLn (renderPretty $ pretty p') >> putStrLn (renderPretty $ pretty result)
  ParseLambda {..} -> do
    source <- readFile sourcePath
    case P.runParser LP.expression sourcePath source of
      Left parseError -> putStrLn $ P.parseErrorPretty parseError
      Right e -> putStrLn $ renderPretty $ pretty e

main :: IO ()
main = execParser programOptionsParser >>= dispatchForMode
 where
  programOptionsParser = info (optionsParser <**> helper)
    ( fullDesc <> header "systemM -- A Materialization Calculus"
    )
