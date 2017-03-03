module D3Elm.Geo.Length exposing (..)

import Basics exposing (sqrt)
import D3Elm.Geo.Math exposing (..)


orthodromicLength (lambda0, phi0) (lambda1, phi1) =
  let sinPhi0 = sin phi0
      cosPhi0 = cos phi0
      sinPhi = sin phi1
      cosPhi = cos phi1
      delta = abs(lambda1 - lambda0)
      cosDelta = cos delta
      sinDelta = sin delta
      x = cosPhi * sinDelta
      y = cosPhi0 * sinPhi - sinPhi0 * cosPhi * cosDelta
      z = sinPhi0 * sinPhi + cosPhi0 * cosPhi * cosDelta
  in  atan2 (sqrt(x * x + y * y)) z


cartesianLength (x0, y0, z0) (x1, y1, z1) =
  sqrt ((x1-x0)^2 + (y1-y0)^2 + (z1-z0)^2)



















