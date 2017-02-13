module Shapes.Voronoi.VoronoiSimple exposing (main)

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


main = mainHtml


data2 =
  let xs0 =
        let nx = 1
            ny = 4
            lx = mkList 1 nx
        in L.concatMap (\x -> L.map (\y -> (20 * toFloat x, 20 * toFloat y)) (mkList 1 ny)) lx

      xs1 = L.map (\(x, y) ->
                  let a = pi/7
                  in (x * cos a - y * sin a, x * sin a + y * cos a)) xs0
  in L.map (\(x,y) -> (x+100, y+100)) xs1

data =
--  let xs0 = [(10, 10), (12, 9), (9, 8), (7, 7), (2, 6), (12, 5), (16, 4), (5, 3), (13, 2), (10, 1)]
--  let xs0 = [(10, 10), (12, 9), (13, 6), (13.5, 5.5), (15, 5), (9, 4), (12, 1)] --, (16, 4), (5, 3), (13, 2), (10, 1)]
  --let xs0 = [(10, 10), (12, 9), (13, 6), (15, 5), (9, 4), (12, 1)] --, (16, 4), (5, 3), (13, 2), (10, 1)]
  let xs0 = [
    (0, 0)
    , (0, 1)
    , (0.2, 0.5)
    , (0.3, 0.6)
    , (0.4, 0.48)
    , (0.6, 0.3)
    , (0.7, 0.6)
    , (1, 0)
    , (1, 1)
     ]
  in L.map (\(x,y) -> (10+x*10, y*10)) xs0
--  in xs0


sites =
  let indexes = mkList 0 100
      datas = L.reverse <| L.sortBy second data
  in L.map (\(i, p) -> Site i p) (zip indexes datas)


edges =
  let events = L.map (\p -> PointEvent p) sites
      (rs, vs) = loop [] events
  in vs

log =
  let events = L.map (\p -> PointEvent p) sites
      (rs, vs) = loop [] events
      sts = L.map (\(s, sites, _) -> (s, sites)) rs
  in L.reverse sts


transform (x,y) = (x*20, 600-y*20)

points  data =
  let proc (x,y) = S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "2", SA.fill "blue"] []
  in L.map (proc << transform) data


midPoint a b =
  let (Site _ (xa, ya)) = a
      (Site _ (xb, yb)) = b
      xm = (xa+xb)/2
      ym = (ya+yb)/2
  in (xm, ym)


showEdges edges =
  let es = D.values edges
      proc e =
        case e of
          Edge a b (Just p) Nothing -> -- []
            let (x,y) = transform p
--                (xm, ym) = transform <| midPoint a b
--                path1 = moveTo (x, y) path0
--                path2 = lineTo (xm, ym) path1
--                dstr = path2.thePath
            in [
                --  (S.path [SA.d dstr, SA.stroke "purple", SA.strokeWidth "1", SA.fill "none"] [], "Pt:"++toString (x,y, xm, ym))
                (S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "7", SA.fill "black"] [], "Pt:"++toString (x,y))
                ]
          Edge _ _ (Just ps) (Just pe) ->
            let (xs,ys) = transform ps
                (xe,ye) = transform pe
                path1 = moveTo (xs, ys) path0
                path2 = lineTo (xe, ye) path1
                dstr = path2.thePath
            in [(S.path [SA.d dstr, SA.stroke "red", SA.strokeWidth "1", SA.fill "none"] [], "Peg:"++toString (xs,ys, xe, ye))
                , (S.circle [SA.cx (toString xs), SA.cy (toString ys), SA.r "3", SA.fill "red"] [], "")
                , (S.circle [SA.cx (toString xe), SA.cy (toString ye), SA.r "3", SA.fill "red"] [], "")
                ]
          _ -> [] --Edge _ _ (Just ps) (Just pe) ->

--            in (S.path [] [], "Peg:"++toString (xs,ys, xe, ye))
  in L.concat <| L.map proc es


vpoints  data =
  let proc v =
    case v of
      VertexStart p ->
        let (x,y) = transform p
        in S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "3", SA.fill "green"] []
    --  VertexStart (x,y) -> S.circle [] []
      VertexEnd p ->
        let (x, y) = transform p
        in S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "3", SA.fill "red"] []
  in L.map proc data

showlog sts =
  L.map (\(s, sites) -> Html.ol [] [(Html.text s), (Html.text (toString sites))]) sts

--  <circle cx="100" cy="10" r="2" fill="blue"/>
mainHtml : Html.Html msg
mainHtml =
  let  d = "" --(basis data path0).thePath
  in div []
          (svg  [ width "1000", height "500", viewBox "0 0 1000 600" ]
            ( points data ++ (L.map first <| showEdges edges) )
           :: (
          [ Html.text (toString edges)]
             ++ (L.map (\e -> Html.text (second e ++ "\n")) (  showEdges edges))
             ++[Html.text ("fin")]
             ++ showlog log
             )
              )












