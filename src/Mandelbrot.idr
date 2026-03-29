module Mandelbrot

import Data.Fin
import Data.Vect

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
  Complex Double ->      -- top left
  Complex Double ->      -- bottom right
  Fin imageHeight ->     -- row index
  Fin imageWidth ->      -- column index
  Complex Double         
pixelToComplex topLeft bottomRight row column =
  let pixel        = cast (column,     row)
      imageSize    = cast (imageWidth, imageHeight)
      pixelPercent = pixel       `componentWiseDivide`   imageSize
      planeSize    = bottomRight `subtract`              topLeft
      offset       = planeSize   `componentWiseMultiply` pixelPercent
  in  topLeft `add` offset

allPixels : (height : Nat) -> (width : Nat) -> Vect (height * width) (Fin height, Fin width)
allPixels h w = concat (map (\i => map (\j => (i, j)) (allFins w)) (allFins h))

escapeTime :
  Complex Double ->                     -- z0
  (Complex Double -> Complex Double) -> -- iterative equation to apply to f0
  (Complex Double -> Bool) ->           -- escape condition
  Nat ->                                -- iteration max
  Maybe Nat                             -- escape iteration count
escapeTime z0 f outOfBounds iterationMax = loop z0 0 where
  loop : Complex Double -> Nat -> Maybe Nat
  loop z n =
    if n >= iterationMax
    then Nothing
    else
      let z' = f z in
      if outOfBounds z'
      then Just n
      else loop z' (n + 1)

mandelbrotEscapeTimes :
  (imageHeight : Nat) ->
  (imageWidth  : Nat) ->
  Complex Double -> -- top left of complex plane
  Complex Double -> -- bottom right of complex plane
  Nat ->            -- iteration max
  Vect (imageHeight * imageWidth) (Maybe Nat)
mandelbrotEscapeTimes imageHeight imageWidth topLeft bottomRight iterationMax =
  map pixelToEscapeTime (allPixels imageHeight imageWidth)
  where
    pixelToEscapeTime : (Fin imageHeight, Fin imageWidth) -> Maybe Nat
    pixelToEscapeTime (row, column) =
      let c           = pixelToComplex topLeft bottomRight row column
          z0          = (0.0 :+ 0.0)
          f           = \z => (z `multiply` z) `add` c
          outOfBounds = \z => normSquared z > 4
      in  escapeTime z0 f outOfBounds iterationMax
