module D3Elm.Shapes.Curves.Step exposing (..)

import Basics exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Curves.Linear exposing (..)


step alpha points path =
  case points of
    [] -> path
    [_] -> path
    _ ->
      let go points  =
            case points of
              [] -> []
              [p] -> [p]
              (p0::p3::r) ->
                let (p1, p2) = intermediatePoints alpha p0 p3
                in p0::p1::p2::go (p3::r)
          allPoints = go points
    in linear allPoints path


intermediatePoints alpha (x0, y0) (x3, y3) =
  let ((x1, y1) as p1) = (x0 * (1-alpha) + x3 * alpha, y0)
      ((x2, y2) as p2) = (x1, y3)
  in (p1, p2)







