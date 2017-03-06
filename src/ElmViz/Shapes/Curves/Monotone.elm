module ElmViz.Shapes.Curves.Monotone exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import ElmViz.Path.Path exposing (..)

import List as L exposing (..)
import Tuple  exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Path.Path exposing (..)

eps = 1e-6

sign x = if x < 0 then -1 else 1


slope3 (x0, y0) (x1, y1) (x2, y2) =
  let h0 = x1 - x0
      h1 = x2 - x1
      s0 = (y1 - y0) / h0
      s1 = (y2 - y1) / h1
  in if s0 * s1 <= 0
     then 0
     else let pi = (s0 * h1 + s1 * h0) / (h0 + h1)
          in  if abs pi > 2 * abs s0 || abs pi > 2 * abs s1
              then 2 * sign s1 * (min (abs s0) (abs s1))
              else pi


slope2 (x0, y0) (x1, y1) t =
  let h = x1 - x0
  in if abs h < eps
    then (3 * (y1 - y0)/h - t) / 2
    else t



addPoints (x0, y0) (x1, y1) t0 t1 path =
  let dx = (x1 - x0) / 3
  in cubicCurveTo (x0 + dx, y0 + dx * t0) (x1 - dx, y1 - dx * t1) (x1, y1) path



monotone points path =
  case points of
    [] -> path
    [(x0, y0)] -> moveTo (x0, y0) path
    [(x0, y0), (x1, y1)] -> lineTo (x1, y1) <| moveTo (x0, y0) path
    ((x0, y0)::(x1, y1)::(x, y)::r) ->
      let go points previousSlope path =
              case points of
                ((x0, y0)::(x1, y1)::[]) ->
                  let slope = slope2 (x0, y0) (x1, y1) previousSlope
                      path1 = addPoints (x0, y0) (x1, y1) previousSlope slope path
                  in path1
                ((x0, y0)::(x1, y1)::(x,y)::r) ->
                  let slope = slope3 (x0, y0) (x1, y1) (x, y)
                      path1 = addPoints (x0, y0) (x1, y1) previousSlope slope path
                  in go ((x1, y1)::(x,y)::r) slope path1
                _ -> path
          sl3 = slope3 (x0, y0) (x1, y1) (x, y)
          sl2 = slope2 (x0, y0) (x1, y1) sl3
          path1 = moveTo (x0, y0) path
          path2 = addPoints (x0, y0) (x1, y1) sl2 sl3 path1
      in go ((x1, y1)::(x, y)::r) sl3 path2

