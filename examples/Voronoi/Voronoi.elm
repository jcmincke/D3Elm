module Voronoi.Voronoi exposing (main)

import Debug exposing (..)
--import Html as H exposing (Html, button, div, text)
import Html exposing (Html, button, div, text)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)
import Tuple exposing (..)

import ElmViz.Path.Path exposing (..)
import ElmViz.Common exposing (..)

import ElmViz.Voronoi.Common exposing (..)
import ElmViz.Voronoi.Voronoi exposing (..)

import Voronoi.Data exposing (..)

main = mainHtml



data = filterDuplicateData (L.map (\(x,y) -> (x*2, y*1)) xs0)

checkEvents events0 =
  let eps = 1e-5
      go events =
        case events of
          [] -> True
          [a] -> True
          (a::b::r) ->
            let ya = eventY a
                yb = eventY b
                dy = abs (ya-yb)
            in  if  dy > eps
                then go (b::r)
                else let l = log "same y" (a, b)
                     in False
  in go events0


filterDuplicateData events0 =
  let eps = 1e-2
      go acc events =
        case events of
          [] -> acc
          [a] -> a::acc
          (a::b::r) ->
            let (xa, ya) = a
                (xb, yb) = b
                dx = abs (xa-xb)
                dy = abs (ya-yb)
            in  if dx > eps && dy > eps
                then go (a::acc) (b::r)
                else let l = log "same y" (a, b)
                     in go (acc) (b::r)
  in L.reverse <| (L.sortBy second  <| go [] events0)


cedges = voronoi data


cellMap =
  let c = createCells cedges
      box = Box -0.2 1.2 2.2 -0.2
      c1 = D.map (\_ (Cell site pts) -> Cell site (clipCell box pts)) c
  in c1


transform (x,y) = (10+x*300, 500-y*300)

points  data =
  let proc (x,y) = S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "2", SA.fill "blue"] []
  in L.map (proc << transform) data


midPoint a b =
  let (Site _ (xa, ya)) = a
      (Site _ (xb, yb)) = b
      xm = (xa+xb)/2
      ym = (ya+yb)/2
  in (xm, ym)


showEdges cedges =
  let es = D.values cedges
      showEdge (CEdge _ _ ps pe) =
        let (xs,ys) = transform ps
            (xe,ye) = transform pe
            path1 = moveTo (xs, ys) path0
            path2 = lineTo (xe, ye) path1
            dstr = path2.thePath
        in [(S.path [SA.d dstr, SA.stroke "red", SA.strokeWidth "1", SA.fill "none"] [], "Peg:"++toString (xs,ys, xe, ye))
             , (S.circle [SA.cx (toString xs), SA.cy (toString ys), SA.r "1", SA.fill "red"] [], "")
             , (S.circle [SA.cx (toString xe), SA.cy (toString ye), SA.r "1", SA.fill "red"] [], "")
             ]
  in L.concat <| L.map showEdge es




showCells cellMap =
  let cells = D.values cellMap
      proc (Cell (Site _ pc) points) =
        let go acc points =
                case points of
                  [] -> acc
                  [_] -> acc
                  (ps::pe::r) ->
                    let (xs,ys) = transform ps
                        (xe,ye) = transform pe
                        path1 = moveTo (xs, ys) path0
                        path2 = lineTo (xe, ye) path1
                        dstr = path2.thePath
                    in go ((S.path [SA.d dstr, SA.stroke "red", SA.strokeWidth "1", SA.fill "none"] [])::acc) (pe::r)
        in go [] points
  in L.concat <| L.map proc cells


mainHtml =
  let cellMap42 = D.filter (\i _ -> i==9) cellMap
      l = log "keys " (D.keys cellMap42)
  in div []
          (svg  [ width "1000", height "500", viewBox "0 0 1000 600" ]
            ( points data ++ showCells cellMap)
            :: []
              )

mainHtml2 =
  div []
          (svg  [ width "1000", height "500", viewBox "0 0 1000 600" ]
            ( points data ++ (L.map first <| showEdges cedges) )
            :: []
              )









