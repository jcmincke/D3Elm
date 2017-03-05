module Geo.Clip.Circle exposing (main)

import Html exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)


import GeoJson exposing (..)

import D3Elm.Geo.Transformation exposing (..)
import D3Elm.Geo.Common exposing (..)
import D3Elm.Geo.Projection.Orthographic exposing (..)
import D3Elm.Geo.Projection.Gnomonic exposing (..)
import D3Elm.Geo.Projection.Stereographic exposing (..)
import D3Elm.Geo.Graticule exposing (..)
import D3Elm.Geo.Circle as C exposing (..)
import D3Elm.Geo.Scale as S exposing (..)
import D3Elm.Geo.Rotation as R exposing (..)
import D3Elm.Geo.Clip.Clip exposing (..)
import D3Elm.Geo.Rendering.Simple exposing (..)

import D3Elm.Path.Path as P exposing (..)

main = mainHtml polyGrat

grat : GeoJsonObject
grat =
  let conf = {
    nbParallels = 7
    , nbMeridians = 24
    , nbParallelSteps = 72
    , nbMeridianSteps = 72
    }
  in Geometry (graticule conf)


polyGrat : GeoJsonObject
polyGrat =
  let conf = {
    deltaLambda = pi/8
  , deltaPhi = pi/24
  , lambdaRepeats = [-pi, -3*pi/4, -pi/2, -pi/4, 0, pi/4, pi/2, 3*pi/4]
  , phiRepeats = [-pi/4, 0, pi/4]
  , nbParallelSteps = 10
  , nbMeridianSteps = 10
    }
  in Geometry (polygonialGraticule conf)

renderCtx =
  let rp (x, y) acc =
      (S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "1"] []) :: acc
      rl pts acc =
        case pts of
          [] -> acc
          [_] -> acc
          [_,_] -> acc
          (h::r) -> let path1 = P.moveTo h path0
                        path2 = L.foldl (\p path -> P.lineTo p path) path1 r
                        path3 = path2
                        cmd = S.path [SA.d path3.thePath, SA.stroke "black", SA.strokeWidth "1", SA.fill "none"] []
                    in  cmd :: acc
  in {renderPoint = rp
      , renderLine = rl
      }



--mainHtml : Html.Html msg
mainHtml obj0 =
  let tr = orthographic >> (S.scale 100 100) >> (S.translate 100 100)
      trr = (R.rotate 0.2 0.7 0.2)
      --tr = (R.rotate 0 0.1 0) >> gnomonic >> (S.scale 10 10) >> (S.translate 100 100)
      --tr = (R.rotate 0 0.1 0) >> stereographic >> (S.scale 10 10) >> (S.translate 100 100)
      geoTr = createTransformation tr
      geoTrr = createTransformation trr
      clippingTr = createCircleClippingTransformation (pi/2-epsilon)
      obj1 = (geoTrr obj0)
      mObj2 = clippingTr obj1
  in  case mObj2 of
      Just obj2 ->
        let --obj = geoTr obj1 -- obj1
            obj = geoTr obj2
            cmds = render renderCtx obj []
        in svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
                [ g [SA.transform "translate(200, 200)"]
                    cmds
                ]
      Nothing -> svg [] []









