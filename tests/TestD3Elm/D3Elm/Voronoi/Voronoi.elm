module TestD3Elm.D3Elm.Voronoi.Voronoi exposing (..)

import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)

import D3Elm.Common exposing (..)

import D3Elm.Voronoi.Voronoi  as V exposing (..)
import TestD3Elm.D3Elm.Voronoi.VoronoiRef  as V exposing (..)


eps = 1e-10

all : Test
all =
    describe "Voronoi "
        [ test "2nd degree equation" <| test2ndDegreeEquation
          , test "parabola intersection" <| testParabolaIntersection
          , test "insert arc" <| testInsertArc
        ]

testInsertArc () =
  let s0 = Site 0 (10, 10)
      xs = [(30, 9), (15, 8)] --, (57, 7)] --, (2, 6), (12, 5), (22, 4), (5, 3), (19, 2), (10, 1)]
      indexes = mkList 1 100
      sites = L.map (\(i, p) -> Site i p) (zip indexes xs)
      yps = L.map second xs

      -- tree implementation
      tree0 = Leaf s0
      foldProc1 s acc =
        let (Site _ (_, yp)) = s
        in insertArc s (isMiddleArc yp s) (isBoundaryArc yp s) acc
      tree = L.foldl foldProc1 tree0 sites
      sites1 = allSites tree

      -- reference implementation
      sites0 = [s0]
      foldProc2 s acc =
        let (Site _ (_, yp)) = s
        in insertArcRef s (isMiddleArc yp s) (isBoundaryArc yp s) acc
      sites2 = L.foldl foldProc2 sites0 sites

      r = isMiddleArc 9 (Site 3 (29,9))    (Site 2 (15,10)) (Site 0 (10,10)) (Site 1 (30,10))
  in Expect.equal (Just r) Nothing --sites2
--  in Expect.equal sites1 [] --sites2
--  in Expect.equal sites1 sites2





-- insertArcRef ns intersectPredClosed intersectPredOpen sites =

-- insertArc : site -> (site -> site -> site -> ArcPred) -> (OpenOn -> site -> site -> ArcPred)


{-}
testInsertArc () =
  let ta = Leaf "a"
      pred1 _ _ = V.Ok
      visit acc _ = acc
      tab = insertArc "b" visit () pred1 ta
      pred2 _ s = if s == "b" then V.Ok else OnLeft
      tabc = insertArc "c" visit () pred2 tab
      sites = allSites tabc
      r = Expect.equal sites ["a","b","c","b","a"]
  in r
-}


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
