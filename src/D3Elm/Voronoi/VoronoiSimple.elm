module D3Elm.Voronoi.VoronoiSimple exposing (..)

import Debug exposing (log)

import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)

--import D3Elm.Voronoi.Voronoi as V exposing (Event (..), Site (..), insertEvent, isBoundaryArc, ArcPred (..),
--          isMiddleArc, circumCircle, OpenOn (..))

import D3Elm.Common exposing (..)
import D3Elm.Voronoi.Common exposing (..)

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

type Vertex =
  VertexStart Point
  |VertexEnd Point


type Edge =
  Edge Site Site (Maybe Point) (Maybe Point)
  | BadEdge Site Site String

type alias EdgeMap = D.Dict (Int, Int) Edge

insertArc :
  Site
  -> (Site -> Site -> Site -> ArcPred)
  -> (OpenOn -> Site -> Site -> ArcPred)
  -> List Site
  -> List (Event Site)
  -> EdgeMap
  -> (List Site, List (Event Site), EdgeMap)
insertArc ns intersectPredClosed intersectPredOpen sites0 events0 edges0 =
  let (Site _ (_, yd)) = ns
      go visited sites =
      case sites of
        (a::b::[]) ->
          case intersectPredOpen OpenOnRight a b of
            ArcFound vertex ->
              let edges1 = insertUndefinedEdge b ns edges0
                  events1 = addCircleEvent yd a b ns events0
                  l = log "insert point right" ns
              in (L.reverse visited ++ (a::b::ns::b::[]), events1, edges1)
            _ ->  (sites0, events0, edges0)
        (a::b::c::r) ->
          case intersectPredClosed a b c of
            ArcFound vertex ->
              let edges1 = insertUndefinedEdge b ns edges0
                  events1 = addCircleEvent yd a b ns events0
                  events2 = addCircleEvent yd ns b c events1
                  l = log "insert point middle" ns
              in (L.reverse visited ++ (a::b::ns::b::c::r), events2, edges1)
            _ -> go (a::visited) (b::c::r)
        _ -> (sites0, events0, edges0)
  in case sites0 of
    [] -> ([ns], events0, edges0)
    [a] ->  let edges1 = insertUndefinedEdge a ns edges0
            in ([a,ns,a], events0, edges1)
    (a::b::r) ->
      case intersectPredOpen OpenOnLeft a b of
        ArcFound vertex ->
          let edges1 = insertUndefinedEdge a ns edges0
              events1 = addCircleEvent yd ns a b events0
              l = log "insert point left" ns
          in  (a::ns::a::b::r, events1, edges1)
        _ -> go [] sites0

removeArc : (Float, Float) -> Float -> Site -> Site -> Site -> List Site -> List (Event Site) -> EdgeMap -> (List Site, List (Event Site), EdgeMap)
removeArc pc radius a b c sites0 events0 edges0 =
  let (xc, yc) = pc
      yd = yc - radius
      l = log "removeArc: try " ((a, b, c), siteSummary sites0)
      go acc sites =
          case sites of
            (ap1::a1::b1::c1::cp1::r) ->
              if (a==a1 && b==b1 && c==c1)
              then  let sites1 = L.reverse acc ++ (ap1 :: a1 :: c1 :: cp1 :: r)
                        events1 = addCircleEvent yd ap1 a1 c1 events0
                        events2 = addCircleEvent yd a1 c1 cp1 events1
                        edges1 =  insertEdges [(a,b,pc), (b,c,pc), (a,c,pc)] edges0
                        l = log "removed arc" (yd, siteSummary sites1)
                    in  (sites1, events2, edges1)
              else go (ap1::acc) (a1::b1::c1::cp1::r)

            (ap1::a1::b1::c1::[]) ->
              if (a==a1 && b==b1 && c==c1)
              then  let sites1 = L.reverse acc ++ (ap1 :: a1 :: c1 :: [])
                        events1 = addCircleEvent yd ap1 a1 c1 events0
                        edges1 =  insertEdges [(a,b,pc), (b,c,pc), (a,c,pc)] edges0
                        l = log "removed arc" (yd, siteSummary sites1)
                    in  (sites1, events1, edges1)
              else go (ap1::acc) (a1::b1::c1::[])

            (a1::b1::c1::[]) ->
              if (a==a1 && b==b1 && c==c1)
              then  let sites1 = L.reverse acc ++ (a1 :: c1 :: [])
                        edges1 =  insertEdges [(a,b,pc), (b,c,pc), (a,c,pc)] edges0
                        l = log "removed arc" (yd, siteSummary sites1)
                    in  (sites1, events0, edges1)
              else (sites0, events0, edges0)

            _ ->  (sites0, events0, edges0)

  in case sites0 of
      (a1::b1::c1::[]) ->
        if (a==a1 && b==b1 && c==c1)
        then  let sites1 = (a1 :: c1 :: [])
                  edges1 =  insertEdges [(a,b,pc), (b,c,pc), (a,c,pc)] edges0
                  l = log "removed arc" (yd, siteSummary sites1)
              in  (sites1, events0, edges1)
        else  (sites0, events0, edges0)

      (a1::b1::c1::cp1::r) ->
        if (a==a1 && b==b1 && c==c1)
        then  let sites1 = (a1 :: c1 :: cp1  :: r)
                  events1 = addCircleEvent yd a1 c1 cp1 events0
                  edges1 =  insertEdges [(a,b,pc), (b,c,pc), (a,c,pc)] edges0
                  l = log "removed arc" (yd, siteSummary sites1)
              in  (sites1, events1, edges1)
        else go [] (a1::b1::c1::cp1::r)

      _ ->  (sites0, events0, edges0)

--removeArc : (Float, Float) -> Float -> Site -> Site -> Site -> List Site -> List (Event Site) -> EdgeMap -> (List Site, List (Event Site), EdgeMap)

--removeArc pc radius a b c sites0 events0 edges0 =

--removeArc2 : (Float, Float) -> Float -> List Site -> (List Site, Maybe (Site, Site, Site))

removeArc2 :
    (Float, Float) -> Float -> Site -> Site -> Site -> List Site -> List (Event Site) -> EdgeMap
              -> (List Site, List (Event Site), EdgeMap)
removeArc2 pcenter radius ar br cr sites0 events0 edges0 =
  let (xcenter, ycenter) = pcenter
      yd = ycenter - radius
      eps = 1e-4
      l = log "try to remove arc: "  <| siteSummary sites0
      go acc sites =
          case sites of
            (a::b::c::r) ->
              let (Site ia pa) = a
                  (Site ib pb) = b
                  (Site ic pc) = c
                  im1 = parabolaIntersectionLeftOnly yd pa pb
                  im2 = parabolaIntersectionLeftOnly yd pb pc
                  --l = log "intersection"  (yd, im1, im2, ia, ib, ic)
              in case (im1, im2) of
                (Just xab, Just xbc) ->
                    let yab = parabola yd pb xab
                        ybc = parabola yd pb xbc
                        ok =  abs (xab - xbc) < eps && abs (yab - ybc) < eps
                    in  if ok
                        then  let sites1 = L.reverse acc ++ (a :: c :: r)
                                  l = log "removed arc" (yd, siteSummary [a, b, c])
                                  l1 = log "expected arc" (a, ar)
                                  l2 = log "expected arc" (b, br)
                                  l3 = log "expected arc" (c, cr)

                                  edges1 =  insertEdges [(a,b,pcenter), (b,c,pcenter), (a,c,pcenter)] edges0

                              in (sites1, events0, edges1)
                        else  let l = log "removeArc2: skip" (yd, xab, xbc, yab, ybc, ia, ib, ic)
                              in go (a::acc) (b::c::r)
                _ -> let l = log "removeArc2: no intersection" ()
                     in go (a::acc) (b::c::r)
            _ -> let l = log "skip remove arc" (yd)
                 in (sites0, events0, edges0)

  in go [] sites0

siteSummary sites = L.map (\(Site i _) -> i) sites

-- buggy?

{-
checkCircleEvent2 : Site -> Site -> Site -> (Float, Float) -> Float -> Bool
checkCircleEvent2 a b c (xcenter, ycenter) radius =
  let yd = ycenter - radius
      (Site ia pa) = a
      (Site ib pb) = b
      (Site ic pc) = c
      d1 = parabolaDeriv yd pa xcenter
      d2 = parabolaDeriv yd pb xcenter
      d3 = parabolaDeriv yd pc xcenter
      l = log "derivée" (d1, d2, d3, ia, ib, ic)
  in  d1 >= d2 && d2 >= d3
-}

checkCircleEvent : Site -> Site -> Site -> (Float, Float) -> Float -> Bool
checkCircleEvent a b c (xcenter, ycenter) radius =
  let eps = 1e-3
      yd = ycenter - radius
      (Site _ pa) = a
      (Site _ pb) = b
      (Site _ pc) = c
      im1 = parabolaIntersection yd pa pb
      im2 = parabolaIntersection yd pb pc
  in case (im1, im2) of
      (Just (xab, _), Just (xbc, _)) ->
          let yab = parabola yd pb xab
              ybc = parabola yd pb xbc
              --l = log "circle check 1" <| (a,b,c)
              --l1 = log "circle check 2 " <| (xab, yab, xbc, ybc, xcenter, ycenter, radius)
          in  abs (xab - xcenter) < eps && abs (xbc - xcenter) < eps
              && abs (yab - ycenter) < eps && abs (ybc - ycenter) < eps
      _ -> False

addCircleEvent : Float -> Site -> Site -> Site -> List (Event Site) -> List (Event Site)
addCircleEvent yd a b c events =
  if a == c
  then events
  else  let (Site _ pa) = a
            (Site _ pb) = b
            (Site _ pc) = c
            (xc, yc, radius) = circumCircle pa pb pc
            evt = CircleEvent a b c (xc, yc) radius
            bo = checkCircleEvent a b c (xc, yc) radius

        in  if (yc - radius) <= yd && bo
            then insertEvent eventComparer evt events
            else events

--addCircleEvent yd a b c events =
--  let (Site _ pa) = a
--      (Site _ pb) = b
--      (Site _ pc) = c
--  in  if a /= c
--      then  let (xc, yc, radius) = circumCircle pa pb pc
--                evt = CircleEvent a b c (xc, yc) radius
--            in  if (yc - radius) <= yd
--                then insertEvent eventComparer evt events
--                else events
--      else events

findCircleEvents : Float -> List Site -> List (Event Site)
findCircleEvents yd sites0 =
  let go acc sites =
    case sites of
      (a::b::c::r) ->
        let (Site ia pa) = a
            (Site ib pb) = b
            (Site ic pc) = c
        in  if a /= c
            then  let (xc, yc, radius) = circumCircle pa pb pc
                      evt = CircleEvent a b c (xc, yc) radius
                      bo = checkCircleEvent a b c (xc, yc) radius
                  in  if (yc - radius) <= yd && bo
                      then let l = log "found circle event" (ia, ib, ic, (xc, yc), yd)
                           in go (evt::acc) (b::c::r)
                      else go acc (b::c::r)
            else go acc (b::c::r)
      _ -> acc
  in go [] sites0


showEvent evt =
  case evt of
    (PointEvent (Site i _)) -> "Point " ++ toString i
    (CircleEvent (Site ia _) (Site ib _) (Site ic _) _ _) -> "Circle " ++ toString (ia, ib, ic)

eventComparer e1 e2 =
  let getY e =
    case e of
      PointEvent (Site _ (_, y)) -> y
      CircleEvent _ _ _ (_, y) r -> (y-r)
  in compare (getY e2) (getY e1)

processOneEvent : List Site -> List (Event Site) -> EdgeMap -> Event Site -> (List Site, List (Event Site), EdgeMap)
processOneEvent sites0 remainingEvents edges0 event  =
  let yd = eventY event
  in case event of
      (PointEvent s) ->
        let (Site _ (_, yp)) = s
            (sites1, events1, edges1) = insertArc s (isMiddleArc s) (isBoundaryArc s) sites0 remainingEvents edges0
        in (sites1, events1, edges1)
      (CircleEvent a b c pc radius) ->
        let ok = log "event circle OK" <| checkCircleEvent a b c pc radius
            (sites1, events1, edges1) = removeArc pc radius a b c sites0 remainingEvents edges0
--            (sites1, events1, edges1) = removeArc2 pc radius a b c sites0 remainingEvents edges0
        in (sites1, events1, edges1)

loop : List Site -> List (Event Site) -> EdgeMap
loop sites0 events0 =
  let go sites edges events =
    case events of
      [] -> let edges1 = findlastVertices (-20) sites edges
            in edges1
      (event::revents) ->
        let yd = eventY event
            l = log "pre sites ok" <| (checkSiteSMonotony yd sites, yd, siteSummary sites)
            (sites1, revents1, edges1) = processOneEvent sites revents edges event
--            revents2 =
--              if sites1 /= sites
--              then  let revts1 = removeCircleEvents revents
--                        ces = findCircleEvents yd sites1
--                        revts2 = L.foldl (\ce acc -> insertEvent eventComparer ce acc) revts1 ces
--                    in revts2
--              else revents
            l1 = log "after sites ok" <| (checkSiteSMonotony yd sites1, yd-1, siteSummary sites1)
        in go sites1 edges1 revents1
  in go sites0 empty events0




removeCircleEvents : List (Event site) -> List (Event site)
removeCircleEvents evts0 =
  let go evts =
    case evts of
      [] ->[]
      (h::r) ->
        case h of
          CircleEvent _ _ _ _ _ -> go r
          _ -> h :: go r
  in go evts0


insertEdges : List (Site, Site, Point) -> EdgeMap -> EdgeMap
insertEdges vertices edges =
  let proc (a, b, p) acc  = insertEdge a b p acc
  in L.foldl proc edges vertices



insertEdge : Site -> Site -> Point -> EdgeMap -> EdgeMap
insertEdge a b p edges =
  let (Site ia pa) = a
      (Site ib pb) = b
      (ia1, ib1, a1, b1) = if (ia < ib) then (ia, ib, a, b) else (ib, ia, b, a)
  in case D.get (ia1, ib1) edges of
      Nothing -> D.insert (ia1, ib1) (Edge a1 b1 (Just p) Nothing) edges
      Just (BadEdge _ _ _ ) -> edges -- D.insert (ia1, ib1) (BadEdge a1 b1 (s++"\insert for bad edge")) edges
      Just (Edge a1 b1 p1m p2m) ->
        case p1m of
          Just _ ->
            case p2m of
              Just _ -> log "bad edge" <| D.insert (ia1, ib1) (BadEdge a1 b1 "double second point") edges
              Nothing -> D.insert (ia1, ib1) (Edge a1 b1 p1m (Just p)) edges
          Nothing ->
            case p2m of
              Just _ -> log "bad edge" <| D.insert (ia1, ib1) (BadEdge a1 b1 "no first point but second point") edges
              Nothing -> D.insert (ia1, ib1) (Edge a1 b1 (Just p) Nothing) edges



--insertEdge : Site -> Site -> Point -> EdgeMap -> EdgeMap
--insertEdge a b p edges =
--  let (Site ia pa) = a
--      (Site ib pb) = b
--      (ia1, ib1) = if (ia < ib) then (ia, ib) else (ib, ia)
--  in case D.get (ia1, ib1) edges of
--      Nothing ->
--        D.insert (ia1, ib1) (Edge a b (OpenEdge p)) edges
--      Just (Edge _ _ (OpenEdge ps)) ->
--        if p /= ps
--        then D.insert (ia1, ib1) (Edge a b (ClosedEdge ps p)) edges
--        else edges
--      _ -> edges


insertUndefinedEdge : Site -> Site -> EdgeMap -> EdgeMap
insertUndefinedEdge a b edges =
  let (Site ia pa) = a
      (Site ib pb) = b
  in  if (ia < ib)
      then  case D.get (ia, ib) edges of
            Just _ -> log "bad edge" <| D.insert (ia, ib) (BadEdge a b "undefined edge: doublon") edges
            Nothing -> D.insert (ia, ib) (Edge a b Nothing Nothing) edges
      else D.insert (ib, ib) (BadEdge a b "undefined edge: bad order") edges


findlastVertices yd sites0 edges0 =
  let go edges sites =
    case sites of
      [] -> edges
      [a] -> edges
      (a::b::r) ->
        let (Site _ pa) = a
            (Site _ pb) = b
        in  case parabolaIntersection yd pa pb of
              Nothing -> go edges (b::r)
              Just (x, _) ->
                let y = parabola yd pa x
                    edges1 = insertEdge a b (x, y) edges
                in go edges1 (b::r)
  in go edges0 sites0




checkSiteSMonotony yd sites0 =
  let go xcurrent sites =
    case sites of
      [] -> True
      [_] -> True
      (a::b::r) ->
        let (Site _ pa) = a
            (Site _ pb) = b
        in  case parabolaIntersection yd pa pb of
              Nothing -> False
              Just (x, _) ->
                if x >= xcurrent
                then go x (b::r)
                else  let l = log "site false: " (xcurrent, x)
                      in False
  in  go -100000000 sites0

eventY event =
        case event of
          PointEvent (Site _ (_, y)) -> y
          CircleEvent _ _ _ (_, y) r -> (y-r)

checkEventsOrder events0 =
  let go ycurrent events =
        case events of
          [] -> True
          [_] -> True
          (a::b::r) ->
            let ya = eventY a
                yb = eventY b
            in  if yb <= ya
                then go yb (b::r)
                else False

  in  go 100000000 events0



{-}

-}



