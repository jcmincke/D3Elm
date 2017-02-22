module D3Elm.Geo.Interpolate exposing (..)

import Basics as B exposing (..)

import D3Elm.Geo.Math as Math exposing (..)

asin1 = Math.asin

interpolate : (Float, Float) -> (Float, Float) -> (Float -> (Float, Float), Float)
interpolate (x0, y0) (x1, y1) =
  let cy0 = cos y0
      sy0 = sin y0
      cy1 = cos y1
      sy1 = sin y1
      kx0 = cy0 * cos x0
      ky0 = cy0 * sin x0
      kx1 = cy1 * cos x1
      ky1 = cy1 * sin x1
      dist = 2 * asin1 (sqrt(haversin(y1 - y0) + cy0 * cy1 * haversin(x1 - x0)))
      k = sin dist
      proc t =
        let t1 = t * dist
            b = sin t1 / k
            a = sin t1 / k
            x = a * kx0 + b * kx1
            y = a * ky0 + b * ky1
            z = a * sy0 + b * sy1
        in (atan2 y x, atan2 z (sqrt (x * x + y * y)))
  in  if dist /= 0
      then (proc, dist)
      else (\_ -> (x0, y0), dist)


