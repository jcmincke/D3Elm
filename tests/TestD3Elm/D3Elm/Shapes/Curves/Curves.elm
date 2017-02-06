module TestD3Elm.D3Elm.Shapes.Curves.Curves exposing (..)

import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)

import D3Elm.Common exposing (..)

import D3Elm.Shapes.Curves.Natural  exposing (..)



all : Test
all =
    describe "Curves "
        [  test "triDiagonal" <| testTriDiagonal
          , test "checkD" <| testCheckD
          , test "natural" <| testNatural

        ]

testCheckD () =
  let a1 = 0
      a2 = 3
      a3 = 2
      b1 = -1
      b2 = 2
      b3 = 1
      c1 = 3
      c2 = 5
      c3 = 0
      x1 = 1
      x2 = 3
      x3 = 2
      d1 = b1*x1 + c1*x2
      d2 = a2*x1 + b2*x2 + c2*x3
      d3 = a3*x2 + b3*x3
      a = [a1, a2, a3]
      b = [b1, b2, b3]
      c = [c1, c2, c3]
      d = [d1, d2, d3]
      x = [x1, x2, x3]

      cp1 = c1/b1
      cp2 = c2 /(b2 - a2 * cp1)
      cp = [cp1, cp2, 0]
      cpr = sweepCs a b c

      dp1 = d1/b1
      dp2 = (d2 - a2 * dp1) / (b2 - a2 * cp1)
      dp3 = (d3 - a3 * dp2) / (b3 - a3 * cp2)
      dp = [dp1, dp2, dp3]
      dpr = sweepDs a b cpr d

      dc = checkD a b c x
      xr = solve a b c dc
  in Expect.equal dp dpr

solve a b c d =
  let c1 = sweepCs a b c
      d1 = sweepDs a b c1 d
  in backSubstitution c1 d1


testTriDiagonal () =
  let a = [0, 4, 2, 6, 5, 2]
      b = [1, 2, 2, 1, -1, 2]
      c = [2, 1, 5, 4, 1, 0]
      x = [1, 2, 1, -1, 1, 5]
      d = checkD a b c x

      c1 = sweepCs a b c
      d1 = sweepDs a b c1 d
      x1 = backSubstitution c1 d1
      deltas = zipWith (\a b -> abs (a-b) ) x x1
  in Expect.true "Independant members are equals" (L.length x == L.length x1 && L.all (\d -> d <= 1e-6) deltas)



testNatural () =
  let ks = [1,-1,3,4,5]
      a = initialAs ks
      b = initialBs ks
      c = initialCs ks
      d = initialDs ks

      c1 = sweepCs a b c
      d1 = sweepDs a b c1 d
      p1 = backSubstitution c1 d1
      dr = checkD a b c p1

      p2 = computeP2 ks p1
      ks1 = computeKs p1 p2

      deltas = zipWith (\a b -> abs (a-b) ) ks ks1
  in Expect.true "control points  are equals" (L.length ks == L.length ks1 && L.all (\d -> d <= 1e-6) deltas)

