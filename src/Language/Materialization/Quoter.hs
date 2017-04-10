module Language.Materialization.Quoter (
  mP,
  mL
)

where

import Data.Data
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Language.Materialization.Parser

mP :: QuasiQuoter
mP = QuasiQuoter { quoteExp = quotePExp program }

quotePExp :: Data a => Parser a -> String -> TH.Q TH.Exp
quotePExp p s = dataToExpQ (const Nothing) (forceParser p s)

mL :: QuasiQuoter
mL = QuasiQuoter { quoteExp = quotePExp leftExpression }
