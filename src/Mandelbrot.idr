module Mandelbrot

import Data.Fin

import Complex

pixelToComplex :
  {imageHeight : Nat} ->
  {imageWidth  : Nat} ->
  Fin imageHeight ->
  Fin imageWidth ->
  Complex Double ->
  Complex Double ->
  Complex Double
pixelToComplex pixelY pixelX topLeft bottomRight =
  let pixel = map cast (finToInteger pixelX :+ finToInteger pixelY)
      imageSize = map cast (imageWidth :+ imageHeight)
      pixelPercent = elementWiseDivide pixel imageSize
      planeSize = subtract bottomRight topLeft
      offset = elementWiseMultiply planeSize pixelPercent
  in  add topLeft offset
