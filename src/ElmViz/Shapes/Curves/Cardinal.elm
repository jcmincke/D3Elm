module ElmViz.Shapes.Curves.Cardinal exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import ElmViz.Path.Path exposing (..)

import List as L exposing (..)
import Tuple  exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Path.Path exposing (..)


addPoints tension (x0, y0) (x1, y1) (x2, y2) (x, y) path =
  let k = (1 - tension) / 6
      p1x = x1 + k * (x2 - x0)
      p1y = y1 + k * (y2 - y0)
      p2x = x2 + k * (x1 - x)
      p2y = y2 + k * (y1 - y)
  in cubicCurveTo (p1x, p1y) (p2x, p2y) (x2, y2) path


cardinal tension points path =
  case points of
    [] -> path
    [(x0, y0)] -> moveTo (x0, y0) path
    [(x0, y0), (x1, y1)] -> lineTo (x1, y1) <| moveTo (x0, y0) path
    ((x0, y0)::(x1, y1)::(x2, y2)::r) ->
      let points1 = (x1, y1) :: points
          go points path =
              case points of
                ((x0, y0)::(x1, y1)::(x2, y2)::(x,y)::[]) ->
                  let path1 = addPoints tension (x0, y0) (x1, y1) (x2, y2) (x,y) path
                      path2 = addPoints tension (x1, y1) (x2, y2) (x,y) (x1,y1) path1
                  in path2 --lineTo (x2, y2) path1
                ((x0, y0)::(x1, y1)::(x2, y2)::(x,y)::r) ->
                  let path1 = addPoints tension (x0, y0) (x1, y1) (x2, y2) (x,y) path
                  in go ((x1, y1)::(x2, y2)::(x,y)::r) path1
                _ -> path
      in go points1 (moveTo (x0, y0) path)


