module D3Elm.Shapes.Curves.Bundle exposing (..)

import Basics exposing (..)
import List as L exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Curves.Basis exposing (..)


bundle beta points path =
  case points of
    [] -> path
    [_] -> path
    ((x0, y0)::_) ->
      let len = toFloat <| L.length points - 1
      in  case L.reverse points of
          [] -> path
          ((xn, yn)::_) ->
            let dx = xn - x0
                dy = yn - y0
                go i points =
                  case points of
                    [] -> []
                    ((xi,yi)::r) ->
                      let t = toFloat i / len
                          x = beta * xi + (1-beta) * (x0 + t * dx)
                          y = beta * yi + (1-beta) * (y0 + t * dy)
                      in (x, y) :: go (i+1) r
                allPoints = go 0 points
      in basis allPoints path






