module TestD3Elm.D3Elm.Voronoi.Common exposing (..)

import Debug exposing (..)
import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)

import D3Elm.Common exposing (..)

import D3Elm.Voronoi.Common exposing (..)
import D3Elm.Voronoi.VoronoiSimple as V exposing (..)


eps = 1e-10

all : Test
all =
    describe "Voronoi "
        [ test "2nd degree equation" <| test2ndDegreeEquation
          , test "parabola intersection" <| testParabolaIntersection
          , test "CircumCircle" <| testCircumCircle
--          , test "Box Intersection" <| testBoxIntersection
          , test "Clip Cell" <| testClipCell
        ]


testClipCell () =
  let box = Box -10 10 10 -10
      points1 = [
        (11, 0)
        , (0, 11)
        , (-11, 0)
        , (0, -11)
        , (11, 0)
        ]
      points2 = [
        (10, 0)
        , (0, 10)
        , (-10, 0)
        , (0, -10)
        , (10, 0)
        ]
      points = [
        (100, 0)
        , (0, 100)
        , (-100, 0)
--        , (0, -10)
        , (100, 0)
        ]
--      points1 = log "cell" <| orderByAnglesAndClose (0, 0) points
      cells = clipCell box points2
  in Expect.equal cells []

{-}
testBoxIntersection () =
  let box = Box (-10) 10 10 (-10)
      cases = [
        (0, 0, 11, 0.1, BoxSideRight)
        , (0, 0, 10.01, 0.1, BoxSideRight)
        , (0, 0, -11, 0.1, BoxSideLeft)
        , (0, 0, -10.001, 0.1, BoxSideLeft)

        , (0, 0, 0.1, 11, BoxSideTop)
        , (0, 0, 0.1, 10.001, BoxSideTop)
        , (0, 0, 0.1, -11, BoxSideBottom)
        , (0, 0, 0.1, -10.001, BoxSideBottom)
        ]
      rs = L.map (\(x1, y1, x2, y2, side) -> testOneBox box side (x1, y1) (x2, y2)) cases
      r = L.foldl (&&) True rs
  in Expect.true "Box Intersection" r

testOneBox box side p1 p2 =
  let eps = 1e-7
      (x1, y1) = p1
      (x2, y2) = p2
      xFormula x = (x-x1)/(x2-x1)
      yFormula y = (y-y1)/(y2-y1)
      r1 = intersectBox box p1 p2
  in case r1 of
      Intersection s p ->
        let (x, y) = p
            tx = xFormula x
            ty = yFormula y
            r2 = findSide box p
        in (r1  == r2 && abs (tx - ty) < eps && 0 <= tx && tx <= 1 && s == side )
      NoIntersection -> False

-}

type IPoint =
  InsidePoint Point
  |OusidePoint Point
  |BoundaryPoint BoxSide Direction Point



findSide (Box xtl ytl xbr ybr) p =
  let (x, y) = p
  in  if x == xtl
      then if ybr <= y && y <= ytl
           then Intersection BoxSideLeft p
           else NoIntersection
      else  if x == xbr
            then if ybr <= y && y <= ytl
                 then Intersection BoxSideRight p
                 else NoIntersection
            else  if y == ytl
                  then if xtl <= x && x <= xbr
                       then Intersection BoxSideTop p
                       else NoIntersection
                  else  if y == ybr
                        then if xtl <= x && x <= xbr
                             then Intersection BoxSideBottom p
                             else NoIntersection
                        else NoIntersection



--type BoxIntersection =
--  NoIntersection
--  |Intersection BoxSide (Float, Float)


--type BoxSide =
--  BoxSideLeft
--  |BoxSideTop
--  |BoxSideRight
--  |BoxSideBottom




testCircumCircle () =
  let eps = 1e-6
      ((xp1, yp1) as p1) = (1, 0.001)
      ((xp2, yp2) as p2) = (-1.001, 0)
      ((xp3, yp3) as p3) = (0.001, 1)
      (xc, yc, rc) = circumCircle p2 p3 p1
      (xc1, yc1, rc1) = circumCircle p3 p2 p1
      dx = abs (xc - xc1)
      dy = abs (yc - yc1)
      dr = abs (rc - rc1)
  in Expect.true "circum circle" (dx < eps && dy < eps && dr < eps)

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
