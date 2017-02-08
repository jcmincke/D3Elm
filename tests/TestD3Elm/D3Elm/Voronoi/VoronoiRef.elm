module TestD3Elm.D3Elm.Voronoi.VoronoiRef exposing (..)

import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)

import D3Elm.Voronoi.Voronoi as V exposing (..)

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













