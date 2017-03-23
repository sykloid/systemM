module Language.Materialization.Quoter where

import Data.Generics
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote

import Language.Materialization.Core
import Language.Materialization.Parser

mP :: QuasiQuoter
mP = QuasiQuoter
  { quoteExp = quoteProgramExp
  , quotePat = _
  , quoteDec = _
  , quoteType = _
  }

quoteProgramExp :: String -> TH.Q TH.Exp
quoteProgramExp s = dataToExpQ (const Nothing) (runProgramParser s)
