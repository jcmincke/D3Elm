module D3Elm.Geo.Cartesian exposing (..)

import D3Elm.Geo.Math as Math exposing (..)

asin1 = Math.asin


spherical : (Float, Float) -> (Float, Float)
spherical (x,y) = (atan2 x y, asin1 y)

cartesian : (Float, Float) -> (Float, Float, Float)
cartesian (lambda, phi) =
  let cosPhi = cos phi
  in  (cosPhi * cos lambda , cosPhi * sin lambda , sin phi )


