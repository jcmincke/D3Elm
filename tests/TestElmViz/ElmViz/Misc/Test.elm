module TestElmViz.ElmViz.Misc.Test exposing (..)

import Debug exposing (..)
import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (floatRange, tuple, tuple5, constant)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)
import Round as R exposing (..)


--import ElmViz.Common exposing (..)
import ElmViz.Geo.Common exposing (..)
import ElmViz.Geo.Math exposing (..)
import ElmViz.Geo.Cartesian exposing (..)
import ElmViz.Geo.Graticule exposing (..)

import ElmViz.Geo.Rotation exposing (..)
import ElmViz.Geo.Circle exposing (..)
import ElmViz.Geo.Rotation as R exposing (..)



all : Test
all =
    describe "Misc"
        [ --test "test" <| atest
        ]



c =
  let phis  = mkList 0  (pi/4) [] 10
      rot = R.rotate 1 2 0
      pts = closingMap (\phi -> rot (0, phi)) phis
  in pts




atest () =
  let tr l = L.map (\(x,y) -> (x/pi*180, y/pi*180)) l
      go c =
        case c of
          ((x0, y0) :: (x1, y1) :: r) -> (x1-x0, y1-y0) :: go ((x1, y1) :: r)
          _ -> []
      c1 = go c
  in Expect.equal (tr c1) []

