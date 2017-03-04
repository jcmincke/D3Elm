module TestD3Elm.D3Elm.Geo.Clip.Circle1 exposing (..)

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
import D3Elm.Common exposing (..)
import D3Elm.Geo.Math exposing (..)
import D3Elm.Geo.Cartesian exposing (..)
import D3Elm.Geo.Clip.Circle exposing (..)
import D3Elm.Geo.Length exposing (..)
import D3Elm.Geo.Graticule exposing (..)
import D3Elm.Geo.Clip.Clip exposing (..)

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



