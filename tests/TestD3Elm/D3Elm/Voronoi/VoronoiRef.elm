module TestD3Elm.D3Elm.Voronoi.VoronoiRef exposing (..)

import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)

import D3Elm.Voronoi.Voronoi as V exposing (Event (..), Site (..), insertEvent, isBoundaryArc, ArcPred (..),
          isMiddleArc, circumCircle, OpenOn (..))

{-}
type alias Point = {
    x : Float
    , y : Float
  }

type Site = Site Int Point


type Tree site =
  Break (Tree site) (Tree site) site site
  | Leaf site


type ArcPred =
  ArcFound
  |OnLeft
  |Onright
-}

-- insertArc : site -> (site -> site -> site -> ArcPred) -> (OpenOn -> site -> site -> ArcPred)
--   -> Tree site -> Tree site


insertArcRef ns intersectPredClosed intersectPredOpen sites =
  let go visited sites =
    case sites of
      (a::b::[]) ->
        case intersectPredOpen OpenOnRight a b of
          ArcFound -> L.reverse visited ++ (a::b::ns::b::[])
          _ ->  L.reverse visited ++ (a::b::[])
      (a::b::c::r) ->
        case intersectPredClosed a b c of
          ArcFound -> L.reverse visited ++ (a::b::ns::b::c::r)
          _ -> go (a::visited) (b::c::r)
      _ -> L.reverse visited ++ sites
  in case sites of
    [] -> []
    [a] -> [a,ns,a]
    (a::b::r) ->
      case intersectPredOpen OpenOnLeft a b of
        ArcFound -> (a::ns::a::b::r)
        _ ->  go [] sites

removeArc : site -> site -> site -> List site -> List site
removeArc a b c sites0 =
  let go acc sites =
    case sites of
      (a1::b1::c1::r) ->
        if (a==a1 && b==b1 && c==c1)
        then L.reverse acc ++ (a1 :: c1 :: r)
        else go (a1::acc) (b1::c1::r)
      _ -> sites0
  in go [] sites0


findCircleEvents : List Site -> List (Event Site)
findCircleEvents sites =
  let go acc sites =
    case sites of
      (a::b::c::r) ->
        let (Site _ pa) = a
            (Site _ pb) = b
            (Site _ pc) = c
            (xc, yc, radius) = circumCircle pa pb pc
            evt = CircleEvent a b c (xc, yc) radius
        in go (evt::acc) (b::c::r)
      _ -> acc
  in go [] sites


showEvent evt =
  case evt of
    (PointEvent (Site i _)) -> "Point " ++ toString i
    (CircleEvent (Site ia _) (Site ib _) (Site ic _) _ _) -> "Point " ++ toString (ia, ib, ic)

eventComparer e1 e2 =
  let getY e =
    case e of
      PointEvent (Site _ (_, y)) -> y
      CircleEvent _ _ _ (_, y) r -> (y-r)
  in compare (getY e1) (getY e2)

processOneEvent sites event =
  case event of
    (PointEvent s) ->
      let (Site _ (_, yp)) = s
          sites1 = insertArcRef s (isMiddleArc yp s) (isBoundaryArc yp s) sites
      in (showEvent event, sites1)
    (CircleEvent a b c _ _ ) ->
      let sites1 = removeArc a b c sites
      in (showEvent event, sites1)



loop sites0 events =
  let go acc sites events =
    case events of
      [] -> ("End", sites, events)
      (event::r) ->
        let (msg, sites1) = processOneEvent sites event
            circleEvents = findCircleEvents sites1
            events1 = L.foldl (\e acc -> insertEvent eventComparer e acc) r circleEvents
        in go ((msg, sites1, events1)::acc) sites1 events1
  in go [] sites0 events


-- insertEvent comparer evt evts0 =




