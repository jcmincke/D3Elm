module Shapes.Voronoi.VoronoiSimple exposing (main)

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


main = mainHtml

--[Log] bad circle: CircleEvent (Site 0 (15,10)) (Site 4 (14,4.8)) (Site 5 (16,3))
--(17.611475409836064,6.801639344262295) 4.129081580733101 (VoronoiSimple.elm, line 3960)


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
      (0.40855301707725,0.29502968269982643)
      , (0.9031194003503191,0.29360313180345665)
      , (0.9097062720910232,0.4047682837482891)
      , (0.24379409435420163,0.542171827852618)
      , (0.5365227357839395,5.76074675144278e-2)
      , (0.9893296880361802,3.6925370054741835e-2)
      , (0.5966995482755528,0.6374014881568215)
      , (0.46423914700575386,0.8657825040341149)
      , (0.9592260967894903,0.3064856581982369)
      , (0.7718688141626973,0.23341633296916275)
      , (0.35556247212726344,0.3710396637779023)
      , (0.11708350368329401,0.42564340108370335)
      , (0.31693195866055346,9.332404710033515e-2)
      , (0.30254063607016946,0.5979299413609287)
      , (0.2876859751148644,0.967415461868147)
      , (0.6022468580086762,0.4755571364729243)
      , (0.9552649386942111,0.20325522992844203)
      , (0.8886499440094957,0.3103967657637354)
      , (0.735144649201807,0.852251273336184)
      , (0.27002744383703625,0.11006363654128504)
      , (0.9875424460974371,0.9185624876786147)
      , (0.21957448177973715,0.9419681699462321)
      , (0.42016379174991303,0.8109389541959977)
      , (7.572977560707694e-2,0.7171009361394347)
      , (0.8198426634451157,0.37197617601318544)
      , (0.5212183209865082,0.7399136542938065)
      , (0.2657584140665967,9.964216432611916e-4)
      , (0.7780260037942086,0.4719022184327899)
      , (0.6531726428966724,0.5143144234517325)
      , (0.7807787863081597,0.5058218561202659)
      , (0.6145637296256419,0.6705272275031389)
      , (0.9924143171137241,0.8073563086806699)
      , (0.23197000981135796,0.3637735754064144)
      , (0.6372546049430938,0.9549406352384238)
      , (0.9435490339020625,0.8703651458911621)
      , (0.5353448044677545,0.20919146663486865)
      , (0.209212952385464,0.6750965316196076)
      , (0.9443595351501604,0.9553821619702938)
      , (0.10331641054283813,0.1206477054150965)
      , (0.407333950449861,1.900655656452832e-2)
      , (0.9490057539867949,0.34504193701308794)
      , (0.7795026035244146,0.22063322482339487)
      , (0.7860488940386479,0.5367176323742398)
      , (7.203220519520093e-2,0.9374726654876469)
      , (0.162897743000872,0.5875210218245466)
      , (0.16579328874255084,0.2625505819884748)
      , (7.88073210070841e-2,0.9087123746261205)
      , (0.7367703255924829,0.7136109606609733)
      , (0.6318708947420815,0.702577291134644)
      , (0.1118401859252498,0.2655749821986386)

     ]
--  let xs0 = [
--    (0.5, 1.5)
--    , (0, 1)
--    , (1.1, 1.00000000000001)
--     ]

--  in L.map (\(x,y) -> (x*100, y*100)) xs0
  in xs0


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


transform (x,y) = (x*500, 550-y*500)

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
                , (S.circle [SA.cx (toString xs), SA.cy (toString ys), SA.r "1", SA.fill "red"] [], "")
                , (S.circle [SA.cx (toString xe), SA.cy (toString ye), SA.r "1", SA.fill "red"] [], "")
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











