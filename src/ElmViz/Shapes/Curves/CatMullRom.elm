module ElmViz.Shapes.Curves.CatMullRom exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import ElmViz.Path.Path exposing (..)

import List as L exposing (..)
import Tuple  exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Path.Path exposing (..)

eps = 1e-6


addPoint (x0, y0) (x1, y1) (x2, y2) (l01_a, l01_2a) (l12_a, l12_2a) (l23_a, l23_2a) (x,y) =
  let (x1p, y1p) =
        if l01_a > eps -- weird, adapted from D3 javascript
        then  let a = 2 * l01_2a + 3 * l01_a * l12_a + l12_2a
                  n = 3 * l01_a * (l01_a + l12_a)
                  nx = (x1 * a - x0 * l12_2a + x2 * l01_2a) / n
                  ny = (y1 * a - y0 * l12_2a + y2 * l01_2a) / n
              in (nx, ny)
        else (x1, y1)
      (x2p, y2p) =
        if l23_a > eps
        then  let b = 2 * l23_2a + 3 * l23_a * l12_a + l12_2a
                  m = 3 * l23_a * (l23_a + l12_a)
                  nx = (x2 * b + x1 * l23_2a - x * l12_2a) / m
                  ny = (y2 * b + y1 * l23_2a - y * l12_2a) / m
              in (nx, ny)
        else (x2, y2)
  in cubicCurveTo (x1p, y1p) (x2p, y2p) (x2, y2)




catMullRom alpha points path =
  case points of
    [] -> path
    [(x0, y0)] -> moveTo (x0, y0) path
    [(x0, y0), (x1, y1)] -> lineTo (x1, y1) <| moveTo (x0, y0) path
--    [(x0, y0), (x1, y1), (x2, y2)] -> path
    ((x0, y0)::(x1, y1)::(x2, y2)::r) ->
      let x01 = x0 - x1
          y01 = y0 - y1
          l01_2a = (x01 * x01 + y01 * y01) ^ alpha
          l01_a = sqrt l01_2a
          l01 = (l01_a, l01_2a)

          path1 = moveTo (x0, y0) path
      in go alpha ((0,0)::points) (0, 0) l01 path1


go alpha points l01 l12 path =
  case points of
    (p0::p1::p2::[]) ->
      addPoint p0 p1 p2 l01 l12 l12 p2 path
    (p0::p1::p2::p::pr) ->
      let (x2, y2) = p2
          (x, y) = p
          x23 = x2 - x
          y23 = y2 - y
          l23_2a = (x23 * x23 + y23 * y23) ^ alpha
          l23_a = sqrt l23_2a
          l23 = (l23_a, l23_2a)
          path1 = addPoint p0 p1 p2 l01 l12 l23 p path
      in go alpha (p1::p2::p::pr) l12 l23 path1
    _ -> path



