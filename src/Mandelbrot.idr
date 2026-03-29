module Mandelbrot

import Data.Fin

import Complex

finToDouble : Fin n -> Double
finToDouble = cast . finToNat

finPairToDoublePair : (Fin height, Fin width) -> (Double, Double)
finPairToDoublePair = bimap finToDouble finToDouble

natPairToDoublePair : (Nat, Nat) -> (Double, Double)
natPairToDoublePair = bimap cast cast

finPairToComplexDouble : (Fin height, Fin width) -> Complex Double
finPairToComplexDouble = fromPair . finPairToDoublePair

natPairToComplexDouble : (Nat, Nat) -> Complex Double
natPairToComplexDouble = fromPair . natPairToDoublePair

Cast (Fin height, Fin width) (Complex Double) where
  cast = finPairToComplexDouble

Cast (Nat, Nat) (Complex Double) where
  cast = natPairToComplexDouble

pixelToComplex :
  {imageHeight : Nat} ->
  {imageWidth  : Nat} ->
  Fin imageHeight ->     -- row index
  Fin imageWidth ->      -- column index
  Complex Double ->      -- top left
  Complex Double ->      -- bottom right
  Complex Double         
pixelToComplex row column topLeft bottomRight =
  let pixel        = cast (column,     row)
      imageSize    = cast (imageWidth, imageHeight)
      pixelPercent = pixel       `componentWiseDivide`   imageSize
      planeSize    = bottomRight `subtract`              topLeft
      offset       = planeSize   `componentWiseMultiply` pixelPercent
  in  topLeft `add` offset
