module Mandelbrot

import Data.Fin

import Complex

finToDouble : Fin n -> Double
finToDouble = cast . finToNat

finPairToNatPair : (Fin height, Fin width) -> (Nat, Nat)
finPairToNatPair = bimap finToNat finToNat

finPairToDoublePair : (Fin height, Fin width) -> (Double, Double)
finPairToDoublePair = bimap finToDouble finToDouble

Cast (Fin height, Fin width) (Nat, Nat) where
  cast = finPairToNatPair

Cast (Fin n) Double where
  cast = finToDouble

Cast (Fin height, Fin width) (Double, Double) where
  cast = finPairToDoublePair


pixelToComplex :
  {imageHeight : Nat} ->
  {imageWidth  : Nat} ->
  Fin imageHeight ->
  Fin imageWidth ->
  Complex Double ->
  Complex Double ->
  Complex Double
pixelToComplex pixelY pixelX topLeft bottomRight =
  let pixel = map cast (finToNat pixelX :+ finToNat pixelY)
      imageSize = map cast (imageWidth :+ imageHeight)
      pixelPercent = elementWiseDivide pixel imageSize
      planeSize = subtract bottomRight topLeft
      offset = elementWiseMultiply planeSize pixelPercent
  in  add topLeft offset
