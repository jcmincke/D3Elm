module D3Elm.Shapes.Curves.Basis exposing (..)

import Basics exposing (..)

import D3Elm.Path.Path exposing (..)



basis points path =
  case points of
    (p0::p1::[]) ->
      let path1 = moveTo p0 path
      in lineTo p1 path1
    (p0::p1::p2::r) ->
      let path1 = moveTo p0 path
          (x0, y0) = p0
          (x1, y1) = p1
          xn = (5 * x0 + x1) / 6
          yn = (5 * y0 + y1) / 6
          path2 = lineTo (xn, yn) path1
          path3 = addPoint p0 p1 p2 path2
          go points path =
            case points of
              (p0::p1::[]) ->
                let path1 = addPoint p0 p1 p1 path
                in lineTo p1 path1
              (p0::p1::p::r) ->
                let path1 = addPoint p0 p1 p path
                in go (p1::p::r) path1
              _ -> path
      in go (p1::p2::r) path3
    _ -> path



addPoint (x0, y0) (x1, y1) (x,y) path =
  let xb1 = (2 * x0 + x1)/3
      yb1 = (2 * y0 + y1)/3
      xb2 = (x0 + 2 * x1) /3
      yb2 = (y0 + 2 * y1) /3
      xb3 = (x0 + 4 * x1 + x)/6
      yb3 = (y0 + 4 * y1 + y)/6
  in cubicCurveTo (xb1, yb1) (xb2, yb2) (xb3, yb3) path
