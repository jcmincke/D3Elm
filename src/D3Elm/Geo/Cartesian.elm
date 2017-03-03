module D3Elm.Geo.Cartesian exposing (..)

import D3Elm.Geo.Math as Math exposing (..)

asin1 = Math.asin


spherical : (Float, Float, Float) -> (Float, Float)
spherical (x, y, z) = (atan2 y x, asin1 z)




cartesian : (Float, Float) -> (Float, Float, Float)
cartesian (lambda, phi) =
  let cosPhi = cos phi
  in  (cosPhi * cos lambda , cosPhi * sin lambda , sin phi )


