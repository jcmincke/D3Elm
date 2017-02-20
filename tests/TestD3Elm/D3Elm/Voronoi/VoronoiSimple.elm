module TestD3Elm.D3Elm.Voronoi.VoronoiSimple exposing (..)

import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get, empty)
import Tuple exposing (..)

import D3Elm.Common exposing (..)

import D3Elm.Voronoi.Common exposing (..)
import D3Elm.Voronoi.VoronoiSimple as V exposing (..)


eps = 1e-10

all : Test
all =
    describe "Voronoi "
        [
        --test "testVoronoi" <| testVoronoi
        ]

{-}
testVoronoi () =
  let s0 = Site 0 (10, 10)
      xs0 = [(12, 9), (9, 8), (77, 7), (2, 6), (12, 5), (16, 4), (5, 3), (13, 2), (10, 1)]
     -- xs0 = [
     --       (0, 0)
     --       , (0, 1)
     --       , (0.2, 0.5)
     --       , (0.3, 0.6)
     --       , (0.4, 0.48)
     --       , (0.6, 0.3)
     --       , (0.7, 0.6)
     --       , (1, 0)
     --       , (1, 1)
     --        ]
      indexes = mkList 1 100
      xs = s0 :: L.map (\(i, p) -> Site i p) (zip indexes xs0)

      events = L.map (\p -> PointEvent p) xs
      rs = loop [] events
  in Expect.equal 1 1 --rs ([], empty)
   --sites2
--  in Expect.equal sites1 [] --sites2
--  in Expect.equal sites1 sites2
-}

