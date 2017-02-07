module TestD3Elm.D3Elm.Voronoi.Voronoi exposing (..)

import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)

import D3Elm.Common exposing (..)

import D3Elm.Voronoi.Voronoi  exposing (..)


eps = 1e-10

all : Test
all =
    describe "Voronoi "
        [ test "2nd degree equation" <| test2ndDegreeEquation
          , test "parabola intersection" <| testParabolaIntersection
        ]

testParabolaIntersection () =
  let ((xp1, yp1) as p1)= (3, 3)
      ((xp2, yp2) as p2) = (4, 5)
      yd = 2
      rm = parabolaIntersection yd p1 p2
  in case rm of
        Nothing -> Expect.fail "no intersection"
        Just (x1, x2) ->
          let y11 = parabola yd p1 x1
              y21 = parabola yd p2 x1

              y12 = parabola yd p1 x2
              y22 = parabola yd p2 x2
              d1 = abs (y11-y21)
              d2 = abs (y12-y22)
          in Expect.true "1 intersection" (d1 <= eps && d2 <= eps)



test2ndDegreeEquation () =
  let a = 3
      b = 4
      c = 1
      f x = a * x * x + b * x + c
      r = solve2 a b c
  in case r of
      Just (x1, x2) ->
        let e1 = f x1
            e2 = f x2
        in Expect.true "2 solutions" (x2 /= x1 && abs e1 <= eps && abs e2 < eps)
      Nothing -> Expect.fail "no solution"
