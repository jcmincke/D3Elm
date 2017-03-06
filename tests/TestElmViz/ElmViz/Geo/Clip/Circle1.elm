module TestElmViz.ElmViz.Geo.Clip.Circle1 exposing (..)

import Debug as D exposing (..)
import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (floatRange, tuple, tuple5, constant)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)
import Round as R exposing (..)

import GeoJson exposing (..)


import Debug as D exposing (..)
import ElmViz.Common exposing (..)
import ElmViz.Geo.Math exposing (..)
import ElmViz.Geo.Cartesian exposing (..)
import ElmViz.Geo.Clip.Circle exposing (..)
import ElmViz.Geo.Length exposing (..)
import ElmViz.Geo.Graticule exposing (..)
import ElmViz.Geo.Clip.Clip exposing (..)

eps = 1e-3

all : Test
all =
    describe "Circle Clipping "
        [ test "" testCircleClip2
        ]



testCircleClip2 () =
  let clippingTr = createCircleClippingTransformation (pi/3)
      mObj2 = clippingTr grat
      l = D.log "clipped" mObj2
  in  Expect.true "" True

grat : GeoJsonObject
grat =
  let conf = {
    nbParallels = 7
    , nbMeridians = 12
    , nbParallelSteps = 36
    , nbMeridianSteps = 36
    }
  in Geometry (graticule conf)



