module TestD3Elm.D3Elm.Geo.Graticule exposing (..)

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


import D3Elm.Geo.Math exposing (..)
import D3Elm.Geo.Cartesian exposing (..)
import D3Elm.Geo.Graticule exposing (..)



all : Test
all =
    describe "Graticule "
        [ --test "graticule" <| testGraticule
        ]


testGraticule ()  =
  let conf = {nbParallels = 3, nbMeridians = 2, nbParallelSteps = 8, nbMeridianSteps = 8}
      meridians = graticuleMedidians conf
      parallels = graticuleParallels conf
  in Expect.equal parallels []


-- type alias GraticuleConf = {
--   nbParallels : Float
--   , nbMeridians : Float
--   , nbParallelSteps :: Float
--   , nbMeridianSteps :: Float
--   }
--
