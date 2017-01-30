module TestD3Elm.D3Elm.Hierarchy.Treemap.Treemap exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)


import D3Elm.Hierarchy.Treemap.Treemap exposing (..)



testSplitBox () =
  let x1 = 10
      x2 = 20
      iweights = [(1, 2), (2, 2), (3, 4), (4, 5), (5, 7)]
      total = L.foldl (+) 0 <| L.map (\(_, w) -> w) iweights
      r = splitBox x1 x2 iweights
      proc (i, xa, xb) = (i, (xb-xa)/(x2 - x1) * total)
      iweights1 = L.map proc r
  in Expect.equal iweights iweights1

all : Test
all =
    describe "Treemap"
      [ test "splitBox" <| testSplitBox
      ]


