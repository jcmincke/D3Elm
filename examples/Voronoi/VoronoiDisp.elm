module Voronoi.VoronoiDisp exposing (main)

import Debug exposing (..)
--import Html as H exposing (Html, button, div, text)
import Html exposing (Html, button, div, text)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)
import Tuple exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Common exposing (..)

import D3Elm.Voronoi.Common exposing (..)
import D3Elm.Voronoi.VoronoiSimple exposing (..)

--import D3Elm.Voronoi.CommonRef exposing (..)
--import D3Elm.Voronoi.VoronoiSimpleRef exposing (..)
import Voronoi.Edges exposing (..)

main = mainHtml



transform (x,y) = (x*500, 450-y*400)

points xs =
  let proc (x,y) = S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "2", SA.fill "blue"] []
  in L.map (proc << transform) xs


showEdges edges =
  let showEdge ps pe =
        let (xs,ys) = transform ps
            (xe,ye) = transform pe
            path1 = moveTo (xs, ys) path0
            path2 = lineTo (xe, ye) path1
            dstr = path2.thePath
        in [(S.path [SA.d dstr, SA.stroke "red", SA.strokeWidth "1", SA.fill "none"] [], "Peg:"++toString (xs,ys, xe, ye))
             , (S.circle [SA.cx (toString xs), SA.cy (toString ys), SA.r "1", SA.fill "red"] [], "")
             , (S.circle [SA.cx (toString xe), SA.cy (toString ye), SA.r "1", SA.fill "red"] [], "")
             ]

      proc e =
        case e of
          (ps, pe) -> showEdge ps pe

  in L.concat <| L.map proc edges


mainHtml : Html.Html msg
mainHtml =
  let  d = "" --(basis data path0).thePath
  in div []
          (svg  [ width "1000", height "500", viewBox "0 0 1000 600" ]
            ( points xs ++ (L.map first <| showEdges edges) )
            :: []
              )








