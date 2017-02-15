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


insertArcRef ns intersectPredClosed intersectPredOpen edges events sites0 =
  let (Site _ (_, yd)) = ns
      go visited sites =
      case sites of
        (a::b::[]) ->
          case intersectPredOpen OpenOnRight a b of
            ArcFound vertex ->
              let edges1 = insertUndefinedEdge b ns edges
                  events1 = addCircleEvent yd a b ns events
                  l = log "insert point right" ns
              in (L.reverse visited ++ (a::b::ns::b::[]), edges1, events1)
            _ ->  (sites0, edges, events)
        (a::b::c::r) ->
          case intersectPredClosed a b c of
            ArcFound vertex ->
              let edges1 = insertUndefinedEdge b ns edges
                  events1 = addCircleEvent yd a b ns events
                  events2 = addCircleEvent yd ns b c events1
                  l = log "insert point middle" ns
              in (L.reverse visited ++ (a::b::ns::b::c::r), edges1, events2)
            _ -> go (a::visited) (b::c::r)
        _ -> (sites0, edges, events)
  in case sites0 of
    [] -> ([ns], edges, events)
    [a] ->  let edges1 = insertUndefinedEdge a ns edges
            in ([a,ns,a], edges1, events)
    (a::b::r) ->
      case intersectPredOpen OpenOnLeft a b of
        ArcFound vertex ->
          let edges1 = insertUndefinedEdge a ns edges
              events1 = addCircleEvent yd ns a b events
              l = log "insert point left" ns
          in  (a::ns::a::b::r, edges1, events1)
        _ -> go [] sites0

removeArc : Site -> Site -> Site -> List Site -> (List Site, Bool)
removeArc a b c sites0 =
  let go acc sites =
    case sites of
      (a1::b1::c1::r) ->
        if (a==a1 && b==b1 && c==c1)
        then  let sites1 = L.reverse acc ++ (a1 :: c1 :: r)
                  l = log "removed arc" (siteSummary sites1)
              in  (sites1, True)
        else go (a1::acc) (b1::c1::r)
      _ -> (sites0, False)
  in go [] sites0

siteSummary sites = L.map (\(Site i _) -> i) sites

removeArc2 : Float -> List Site -> (List Site, Maybe (Site, Site, Site))
removeArc2 yd sites0 =
  let eps = 1e-4
      l = log "try to remove arc: "  <| siteSummary sites0
      go acc sites =
    case sites of
      (a::b::c::r) ->
        let (Site ia pa) = a
            (Site ib pb) = b
            (Site ic pc) = c
            im1 = parabolaIntersection yd pa pb
            im2 = parabolaIntersection yd pb pc
            --l = log "intersection"  (yd, im1, im2, ia, ib, ic)
        in case (im1, im2) of
          (Just (xab, _), Just (xbc, _)) ->
              let yab = parabola yd pb xab
                  ybc = parabola yd pb xbc
                  ok =  abs (xab - xbc) < eps && abs (yab - ybc) < eps
              in  if ok
                  then  let sites1 = L.reverse acc ++ (a :: c :: r)
                            l = log "removed arc" (yd, siteSummary [a, b, c])
                        in (sites1, Just (a, b, c))
                  else go (a::acc) (b::c::r)
          _ -> go (a::acc) (b::c::r)
      _ -> let l = log "skip remove arc" (yd)
           in (sites0, Nothing)

  in go [] sites0

checkCircleEvent2 : Site -> Site -> Site -> (Float, Float) -> Float -> Bool
checkCircleEvent2 a b c (xcenter, ycenter) radius =
  let yd = ycenter - radius
      (Site _ pa) = a
      (Site _ pb) = b
      (Site _ pc) = c
      d1 = parabolaDeriv yd pa xcenter
      d2 = parabolaDeriv yd pb xcenter
      d3 = parabolaDeriv yd pb xcenter
  in d1 >= d2 && d2 >= d3

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


addCircleEvent yd a b c events =
  let (Site _ pa) = a
      (Site _ pb) = b
      (Site _ pc) = c
      (xc, yc, radius) = circumCircle pa pb pc
      evt = CircleEvent a b c (xc, yc) radius
      -- bo = checkCircleEvent a b c (xc, yc) radius

  in  if (yc - radius) <= yd -- && bo
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
        let (Site _ pa) = a
            (Site _ pb) = b
            (Site _ pc) = c
        in  if a /= c
            then  let (xc, yc, radius) = circumCircle pa pb pc
                      evt = CircleEvent a b c (xc, yc) radius
                  in  if (yc - radius) <= yd
                      then go (evt::acc) (b::c::r)
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

processOneEvent edges sites event remainingEvents =
  case event of
    (PointEvent s) ->
      let (Site _ (_, yp)) = s
          (sites1, edges1, remainingEvents1) = insertArcRef s (isMiddleArc yp s) (isBoundaryArc yp s) edges remainingEvents sites
      in ("insert "++showEvent event, sites1, edges1, remainingEvents1, yp, True)
    (CircleEvent a b c ((xc, yc) as pc) radius) ->
      let yd = (yc - radius)
          (sites1, arcm) = removeArc2 yd sites
      in  case arcm of
            Just _ -> let r =  insertEdges [(a,b,pc), (b,c,pc), (a,c, pc)] edges
                          revts1 = removeCircleEvents remainingEvents
                          ces = findCircleEvents yd sites1
                          revts2 = L.foldl (\ce acc -> insertEvent eventComparer ce acc) revts1 ces
                          --l = log "arc removed : check sites" <| checkSiteSMonotony yd sites1

                      in ("Remove "++showEvent event, sites1, r, revts2, yd, True)
            Nothing -> ("Remove "++showEvent event, sites1, edges, remainingEvents, yd, False)

--      case checkCircleEvent a b c (xc, yc) radius of
--          True ->
--            let yd = (yc - radius)
--                (sites1, wasRemoved) = removeArc a b c sites
--                l = log "removeArc : check sites" <| checkSiteSMonotony yd sites1
--                (edges1, remainingEvents1) =
--                            if wasRemoved
--                            then  let r =  insertEdges [(a,b,pc), (b,c,pc), (a,c, pc)] edges
--                                      revts1 = removeCircleEvents remainingEvents
--                                      ces = findCircleEvents yd sites1
--                                      revts2 = L.foldl (\ce acc -> insertEvent eventComparer ce acc) revts1 ces
--                                  in (r, revts2)
--                            else (edges, remainingEvents)
--            in ("Remove "++showEvent event, sites1, edges1, remainingEvents1, yc - radius, wasRemoved)
--          False -> let _ = log "bad circle" <| event
--                   in ("", sites, edges, remainingEvents, yc - radius, False)
--
loop : List Site -> List (Event Site) -> (List (String,  List Site, List (Event Site)), EdgeMap)
loop sites0 events0 =
  let go (acc, edges) sites events =
    case events of
      [] -> let edges1 = findlastVertices (-20) sites edges
            in (("End", sites, events)::acc, edges1)
      (event::r) ->
        let yd = eventY event
            l = log "pre sites ok" <| (checkSiteSMonotony yd sites, yd, siteSummary sites)
            (msg, sites1, edges1, r1, _, changed) = processOneEvent edges sites event r
            l1 = log "after sites ok" <| (checkSiteSMonotony yd sites, yd-1, siteSummary sites1)
            r3 =  if sites /= sites1
                  then  let r2 = removeCircleEvents r1
                            ces = findCircleEvents yd sites1
                        in L.foldl (\ce acc -> insertEvent eventComparer ce acc) r2 ces
                  else r1
        in go ((msg, sites1, r3)::acc, edges1) sites1 r3
  in go ([], empty) sites0 events0



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








