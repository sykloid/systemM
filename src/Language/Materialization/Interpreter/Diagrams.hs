{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Materialization.Interpreter.Diagrams where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Diagrams.Prelude hiding (output)
import Diagrams.Backend.PGF
import Diagrams.Backend.PGF.Surface
import Diagrams.TwoD.Combinators
import Diagrams.TwoD.Layout.Grid
import System.FilePath
import System.Directory
import System.IO.Temp
import System.Process
import Text.Printf

import qualified Data.Map as M

import Language.Common.PrimitiveValues
import Language.Materialization.Core
import Language.Materialization.Interpreter
import Language.Materialization.Interpreter.Diagrams.Text

type Visualization = Diagram B

type Prescient a = State PrescientState a
type Prescience = (Double, Double)
type PrescientState = M.Map String (Prescience, Prescience)

runPrescient :: Prescient a -> PrescientState -> (a, PrescientState)
runPrescient = runState

fixPrescient :: Prescient a -> a
fixPrescient p = fst $ runPrescient (toFixedPoint p) mempty

toFixedPoint :: Prescient a -> Prescient a
toFixedPoint p = do
  a <- p
  done <- phaseShift
  if done
    then return a
    else toFixedPoint p

phaseShift :: Prescient Bool
phaseShift = do
  m <- get
  let (m', dones) = unzip [((key, (new, new)), old == new) | (key, (old, new)) <- M.toList m]
  put $ M.fromList m'
  return $ and dones

getPrescience :: Diagram B -> Prescience
getPrescience d = (width d, height d)

getExtentsFor :: Diagram B -> String -> Prescient Prescience
getExtentsFor d s = do
  m <- get
  let (current, (w, h)) = fromMaybe ((0, 0), (0, 0)) $ M.lookup s m
  put $ M.insert s (current, (max w (width d), max h (height d))) m
  return current

getBoundingBoxFor :: Diagram B -> String -> Prescient (Diagram B)
getBoundingBoxFor d s = do
  (w, h) <- getExtentsFor d s
  return (strutR2 (V2 w h))

class Visualizable a where
  toViz :: a -> Prescient Visualization

ladidaSurface :: Surface
ladidaSurface = Surface
  { _texFormat = LaTeX
  , _command = "xelatex"
  , _arguments = []
  , _pageSize = Just $ \(V2 w h) ->
      printf "\\usepackage[pass,paperwidth=%smm,paperheight=%smm]{geometry}" (show w) (show h)
  , _beginDoc = "\\begin{document}"
  , _endDoc = "\\end{document}"
  , _preamble = "\\documentclass[crop=true]{standalone}\n\\usepackage{pgfcore}\n\\usepackage{Alegreya}"
  }

visualizeResult :: InterpretationResult Store -> FilePath -> IO ()
visualizeResult ((_, w), _) out = do
  let szSpec = mkSizeSpec2D (Just 800) Nothing
      stores = [store | ClauseEvent _ store _ <- w]
      diagrams = fixPrescient $ forM stores $ \v -> do
        t <- toViz v
        b <- getBoundingBoxFor t "page"
        return $ center t <> center b
  withSystemTempDirectory "systemM" $ \dir -> do
    files <- forM (zip ([0..] :: [Int]) diagrams) $ \(i, d) -> do
      let fName = dir </> show i <.> "pdf"
      renderPGFSurf fName szSpec ladidaSurface d
      return fName
    doesFileExist out >>= \b -> when b (removeFile out)
    callProcess "stapler" ("cat" : files ++ [out])

hdiv :: Double -> [Diagram B] -> Diagram B
hdiv n ds = hsep (n / 2) (map centerY $ intersperse divider ds)
 where
  divider = vrule maxHeight # lw veryThin # lc gray
  maxHeight = maximum (map height ds)

hdiv' :: Double -> [(String, Diagram B)] -> Diagram B
hdiv' n tds = hdiv n $ zipWith heightlet ts ds'
 where
  heightlet s d = pad 1.5 (textSquare s) === d
  (ts, ds) = unzip tds
  ds' = [centerXY $ centerY h <> centerY (strutR2 (V2 (width h) maxHeight)) | h <- ds]
  maxHeight = maximum (map height ds)

vdiv :: Double -> [Diagram B] -> Diagram B
vdiv n ds = vsep (n / 2) (map centerX $ intersperse divider ds)
 where
  divider = hrule maxWidth # lw veryThin # lc gray
  maxWidth = maximum (map width ds)

vdiv' :: Double -> [(String, Diagram B)] -> Diagram B
vdiv' n tds = vdiv n $ zipWith widthlet ts ds'
 where
  widthlet s d = centerX (textSquare s) === d
  (ts, ds) = unzip tds
  ds' = [centerX $ centerX h <> centerX (strutR2 (V2 maxWidth (height h))) | h <- ds]
  maxWidth = maximum (map width ds)

hdist :: String -> Double -> [(String, Diagram B)] -> Prescient (Diagram B)
hdist s x tds = do
  let fs t = s ++ "_" ++ t
      (ts, _) = unzip tds
  ds <- forM tds $ \(t, d) -> do
    b <- getBoundingBoxFor d (fs t)
    return $ centerX d <> centerX b
  return $ hdiv' x (zip ts ds)

vdist :: String -> Double -> [(String, Diagram B)] -> Prescient (Diagram B)
vdist s x tds = do
  let fs t = s ++ "_" ++ t
      (ts, _) = unzip tds
  ds <- forM tds $ \(t, d) -> do
    b <- getBoundingBoxFor d (fs t)
    return $ centerY d <> centerY b
  return $ vdiv' x (zip ts ds)

node :: String -> Diagram B
node s = pad 1.25 $ textSquare ("\\texttt{" ++ s ++ "}")

instance Visualizable Store where
  toViz Store {..} = do
    e <- toViz environment
    i <- toViz idents
    m <- toViz memory
    s <- hdist "store" 1
           [ ("Environment", centerXY e)
           , ("Idents", centerX i)
           , ("Memory", centerXY m)
           ]
    let s' = s # foldl (.) id [connectOutside' (with & headLength .~ verySmall) n (show i) # lc gray
                              | (Name n, Just (Valid (Owned i))) <- M.toList $ globals environment
                              ]
               # foldl (.) id [ connectOutside' (with & headLength .~ verySmall) (show i) (show sa)
                              . connectOutside' (with & headLength .~ verySmall) (show i) (show ha)
                                  # lc gray
                              | (i, Just (Ident _ (Valid sa) (Valid ha))) <- M.toList idents
                              ]
               # foldl (.) id [ connectOutside' (with & headLength .~ verySmall) n (show i) # lc gray
                              | f <- stack environment
                              , (Name n, Just (Valid (Owned i))) <- M.toList (locals f)]
    return $ pad 1.05 $ centerY s'


instance Visualizable Environment where
  toViz Environment {..} = do
    g <- toViz globals
    s <- forM (zip ([1..] :: [Int]) stack) $ \(i, Frame {..}) -> do
      ls <- toViz locals
      cs <- toViz closure
      f <- hdist "frame" 1 [("Locals", ls), ("Closure", cs)]
      return ("Frame: " ++ show i, centerX f)
    s' <- vdist "frame_stack" 1 s
    vdist "environment" 1 [ ( "Globals", centerX g)
                          , ( "Frame Stack", centerX s')
                          ]

instance Visualizable Namespace where
  toViz ns = return $ if M.null ns then mempty else gc
   where
    gc =  gridCat [ node n # fc orange # named n
                  | Name n <- M.keys ns
                  ]

instance Visualizable (Mapping IdentAddress Ident) where
  toViz im = return $ if M.null im then mempty else gc
   where
    gc = gridCat [node (show i) # fc pink # named (show i) | i <- M.keys im]

instance Visualizable Memory where
  toViz Memory {..} = do
    sms <- toViz stackMS
    hms <- toViz heapMS
    vdist "memorySegments" 1
      [ ("Stack", centerXY sms)
      , ("Heap", centerXY hms)
      ]

instance Visualizable (Mapping StackAddress (Nullable StackValue)) where
  toViz sms = do
    vs <- sequence [named (show sa) <$> toViz v | (sa, Just v) <- M.toList sms]
    return $ if M.null sms then mempty else gridCat vs

instance Visualizable (Mapping HeapAddress (Nullable HeapValue)) where
  toViz hms = do
    vs <- sequence [named (show ha) <$> toViz v | (ha, Just v) <- M.toList hms]
    return $ if M.null hms then mempty else gridCat vs

instance Visualizable (Nullable StackValue) where
  toViz nsv = return $ case nsv of
    Null -> node "Φ" # fc orange
    Valid (SmallStackValue (ValueSentinel v)) -> node v # fc teal
    Valid (LargeStackValue (ValueSentinel v)) -> node v # fc teal
    Valid (FunctionStackValue _) -> node "$\\lambda$" # fc teal

instance Visualizable (Nullable HeapValue) where
  toViz nhv = return $ case nhv of
    Null -> node "Φ" # fc orange
    Valid (LargeHeapValue (ValueSentinel v)) -> node v # fc blue
