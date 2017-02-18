module D3Elm.Voronoi.VoronoiSimple exposing (..)

import Debug exposing (log)

import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Set as S exposing (..)
import Maybe exposing (withDefault)
import Tuple exposing (..)

--import D3Elm.Voronoi.Voronoi as V exposing (Event (..), Site (..), insertEvent, isBoundaryArc, ArcPred (..),
--          isMiddleArc, circumCircle, OpenOn (..))

import D3Elm.Common exposing (..)
import D3Elm.Voronoi.Common exposing (..)

--type Vertex =
--  VertexStart Point
--  |VertexEnd Point


type Edge =
  UndefinedEdge Site Site                   -- edge without any know vertices
  |UnBoundedOpenEdge Site Site Point        -- edge with 1st vertex not being a center
  |UnBoundedEdge Site Site Point Point      -- edge with vertices are not centers
  |HalfEdge Site Site Point Point           -- 1st vertex = center, 2nd vertex
  |Edge Site Site Point Point               -- Both vertices are centers
  |OpenEdge Site Site Point                 -- edge whit 1 know vertex (this one being a center)
  |BadEdge Site Site String

type alias EdgeMap = D.Dict (Int, Int) Edge

type CEdge =
  CEdge Site Site Point Point

type alias CEdgeMap = D.Dict (Int, Int) CEdge

transformEdges : EdgeMap -> CEdgeMap
transformEdges edges =
  let proc _ e =
        case e of
        UnBoundedEdge a b ps pe -> CEdge a b ps pe
        HalfEdge a b ps pe -> CEdge a b ps pe
        Edge a b ps pe -> CEdge a b ps pe
        UndefinedEdge a b -> log "c-edge: error : UndefinedEdge" (CEdge a b (0,0) (0,0))
        UnBoundedOpenEdge a b _ -> log "c-edge: error : UnBoundedOpenEdge" (CEdge a b (0,0) (0,0))
        OpenEdge a b _ -> log "c-edge: error : OpenEdge" (CEdge a b (0,0) (0,0))
        BadEdge a b e -> log ("c-edge: error : BadEdge "++e) (CEdge a b (0,0) (0,0))
  in D.map proc edges

type Cell = Cell Site (Set Point)
type alias CellMap = D.Dict Int Cell

createCells cedges =
  let proc (ia, ib) e acc =
        let (CEdge a b ps pe) = e
            acc1 = case D.get ia acc of
                    Just (Cell a pts) ->
                      let pts1 = S.insert ps pts
                          pts2 = S.insert pe pts1
                      in  D.insert ia (Cell a pts2) acc
                    Nothing -> D.insert ia (Cell a (S.fromList [ps, pe])) acc
            acc2 = case D.get ib acc1 of
                    Just (Cell b pts) ->
                      let pts1 = S.insert ps pts
                          pts2 = S.insert pe pts1
                      in  D.insert ib (Cell b pts2) acc1
                    Nothing -> D.insert ib (Cell b (S.fromList [ps, pe])) acc1
        in acc2
  in D.foldl proc D.empty cedges




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
            in  ([a,ns,a], events0, edges1)
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


checkCircleEvent : Site -> Site -> Site -> (Float, Float) -> Float -> Bool
checkCircleEvent a b c (xcenter, ycenter) radius =
  let eps = 1e-8
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
  let go yd sites edges events =
    case events of
      [] -> let l = log "Fini" ()
                edges1 = findlastVertices (yd - 1) sites edges
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
        in go yd sites1 edges1 revents1
  in go 2 sites0 D.empty events0




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

makeSites xs =
  let sxs = L.sortBy second xs
      go i acc xs =
          case xs of
            [] -> acc
            (h::r) -> go (i+1) (Site i h :: acc) r
  in go 0 [] sxs




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
      Just (UndefinedEdge a b) -> D.insert (ia1, ib1) (OpenEdge a b p) edges
      Just (OpenEdge a b p1) -> D.insert (ia1, ib1) (Edge a b p1 p) edges
      Nothing -> D.insert (ia1, ib1) (OpenEdge a b p) edges

      _ -> log "bad edge" <|  edges



inserLastVertex : Site -> Site -> Point -> EdgeMap -> EdgeMap
inserLastVertex a b p edges =
  let (Site ia pa) = a
      (Site ib pb) = b
      (ia1, ib1, a1, b1) = if (ia < ib) then (ia, ib, a, b) else (ib, ia, b, a)
  in case D.get (ia1, ib1) edges of
      Just (UndefinedEdge a1 b1) -> D.insert (ia1, ib1) (UnBoundedOpenEdge a1 b1 p) edges
      Just (UnBoundedOpenEdge a1 b1 p1) -> D.insert (ia1, ib1) (UnBoundedEdge a1 b1 p1 p) edges
      Just (OpenEdge a1 b1 p1) -> D.insert (ia1, ib1) (HalfEdge a1 b1 p1 p) edges
      _ -> log "bad half edge" <| edges


insertUndefinedEdge : Site -> Site -> EdgeMap -> EdgeMap
insertUndefinedEdge a b edges =
  let (Site ia pa) = a
      (Site ib pb) = b
  in  if (ia < ib)
      then  case D.get (ia, ib) edges of
            Just _ -> log "bad edge" edges
            Nothing -> D.insert (ia, ib) (UndefinedEdge a b) edges
      else  insertUndefinedEdge b a edges --M.insert (ib, ib) (BadEdge a b "undefined edge: bad order") edges



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
                    edges1 = inserLastVertex a b (x, y) edges
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



