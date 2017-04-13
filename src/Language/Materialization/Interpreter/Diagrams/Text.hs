{-# LANGUAGE FlexibleContexts #-}

module Language.Materialization.Interpreter.Diagrams.Text (text', textSquare) where

import Diagrams.Prelude

import Diagrams.Backend.PGF

text' :: String -> Diagram PGF
text' s = hboxSurf latexSurface $ "\\vphantom{(}" ++ s

textSquare :: String -> Diagram PGF
textSquare s = textBox # lw none
 where
  t = text' s # center
  w = max (width t) (height t)
  h = height t
  boxShape = scale 1.33 $ roundedRect w h (0.1 * height t)
  textBox = t <> boxShape
