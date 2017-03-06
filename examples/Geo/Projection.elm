module Geo.Projection exposing (main)

import Html exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)


import GeoJson exposing (..)

import ElmViz.Geo.Transformation exposing (..)
import ElmViz.Geo.Common exposing (..)
import ElmViz.Geo.Projection.Orthographic exposing (..)
import ElmViz.Geo.Projection.Gnomonic exposing (..)
import ElmViz.Geo.Projection.Stereographic exposing (..)
import ElmViz.Geo.Graticule exposing (..)
import ElmViz.Geo.Circle as C exposing (..)
import ElmViz.Geo.Scale as S exposing (..)
import ElmViz.Geo.Rotation as R exposing (..)

import ElmViz.Geo.Rendering.Simple exposing (..)

import ElmViz.Path.Path as P exposing (..)

main = mainHtml

grat : GeoJsonObject
grat =
  let conf = {
    nbParallels = 7
    , nbMeridians = 12
    , nbParallelSteps = 36
    , nbMeridianSteps = 36
    }
  in Geometry (graticule conf)

circle : GeoJsonObject
circle =
  let conf = {
    radiusAngle = pi/8
    , nbSteps = 36
    }
  in Geometry (LineString (L.map toGeoPosition (C.circle conf)))



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
                        path3 = closePath path2
                        cmd = S.path [SA.d path3.thePath, SA.stroke "black", SA.strokeWidth "1", SA.fill "none"] []
                    in  cmd :: acc
  in {renderPoint = rp
      , renderLine = rl
      }



mainHtml : Html.Html msg
mainHtml =
  let tr = orthographic >> (S.scale 100 100) >> (S.translate 100 100)
      --tr = (R.rotate 0.5 0.5 0.5) >> orthographic >> (S.scale 100 100) >> (S.translate 100 100)
      --tr = (R.rotate 0 0.1 0) >> gnomonic >> (S.scale 10 10) >> (S.translate 100 100)
      --tr = (R.rotate 0 0.1 0) >> stereographic >> (S.scale 10 10) >> (S.translate 100 100)
      geoTr = createTransformation tr
--      grat1 = geoTr grat
      obj = geoTr circle
      cmds = render renderCtx obj []

  in svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)"]
              cmds
          ]






