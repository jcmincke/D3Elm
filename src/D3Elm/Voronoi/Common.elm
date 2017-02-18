module D3Elm.Voronoi.Common exposing (..)

import Debug exposing (..)
import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)
import Basics exposing (..)


type alias Point = (Float, Float)

type Site = Site Int Point


type Event site =
  PointEvent site
  | CircleEvent site site site Point Float



type ArcPred =
  ArcFound Point
  |OnLeft
  |OnRight

type OpenOn = OpenOnRight | OpenOnLeft


insertEvent : (Event site -> Event site -> Order) -> Event site -> List (Event site) -> List (Event site)
insertEvent comparer evt evts0 =
  let go acc evts =
    case evts of
      [] -> L.reverse (evt::acc)
      (h::r) ->
        case comparer evt h of
          LT -> L.reverse acc ++ (evt::evts)
          GT -> go (h::acc) r
          EQ -> case (evt, h) of
                  (PointEvent _, PointEvent _) -> L.reverse acc ++ (evt::evts)
                  (PointEvent _, CircleEvent _ _ _ _ _) -> L.reverse acc ++ (evt::evts)
                  (CircleEvent _ _ _ _ _, PointEvent _) -> L.reverse acc ++ (h::evt::r)
                  (CircleEvent _ _ _ _ _ as ce1, CircleEvent _ _ _ _ _ as ce2) ->
                    if ce1 == ce2
                    then evts0
                    else L.reverse acc ++ (h::evt::r)
  in go [] evts0


isMiddleArc : Site -> Site -> Site -> Site -> ArcPred
isMiddleArc s a b c =
  let (Site _ ((xa, ya) as pa)) = a
      (Site _ ((xb, yb) as pb)) = b
      (Site _ ((xc, yc) as pc)) = c
      (Site _ (x, y)) = s
      yci = parabola y pc x
  in  if a == c
      then  let yai = parabola y pa x
                ybi = parabola y pb x
            in  if ybi <= yai
                then ArcFound (x, ybi)
                else  if xb < x
                      then OnRight
                      else OnLeft
      else  let imab = parabolaIntersection y pa pb
                imbc = parabolaIntersection y pb pc
            in  case (imab, imbc) of
                  (Just (xab, _), Just (xbc, _)) ->
                    if xab <= x && x <= xbc
                    then  let ybi = parabola y pb x
                          in ArcFound (x, ybi)
                    else  if x < xab
                          then OnLeft
                          else OnRight
                  _ -> log "isMiddleArc: should never reach here" OnLeft  -- should never happen

isBoundaryArc : Site -> OpenOn -> Site -> Site -> ArcPred
isBoundaryArc s side a b =
  case side of
    OpenOnRight ->
      let (Site _ ((xa, _) as pa)) = a
          (Site _ pb) = b
          (Site _ (x, y)) = s
          yai = parabola y pa x
          ybi = parabola y pb x
      in  if xa <= x && ybi <= yai
          then ArcFound (x, ybi)
          else OnLeft
    OpenOnLeft ->
      let (Site _ pa) = a
          (Site _ ((xb, _) as pb)) = b
          (Site _ (x, y)) = s
          yai = parabola y pa x
          ybi = parabola y pb x
      in  if x <= xb && yai <= ybi -- make sure the x is not on the left branch of a
          then ArcFound (x, yai)
          else OnRight




-- parabola equation
parabola yd (xf, yf) x =
  let p = yf - yd
  in (x-xf)^2 / (2 * p) + (yf + yd) / 2

-- first derivative of a parabola
parabolaDeriv yd (xf, yf) x =
  let p = yf - yd
  in (x-xf) / p

-- finding intersection between 2 parabolas p1 and p2
-- returns the left intersection then  the right intersection

parabolaIntersection yd (xp1, yp1) (xp2, yp2) =
  let p1 = yp1 - yd
      p2 = yp2 - yd
      a = p2 - p1
      b = 2 * (p1 * xp2 - p2 * xp1)
      c = p2 * xp1 * xp1 - p1 * xp2 * xp2 + p1 * p2 * (yp1 - yp2)
      rm =  case solve2 a b c of
            Just (x1, x2) ->
              let d1 = parabolaDeriv yd (xp1, yp1) x1
                  d2 = parabolaDeriv yd (xp2, yp2) x1
              in  if d1 >= d2
                  then Just (x1, x2)
                  else Just (x2, x1)
            Nothing -> Nothing
  in rm

parabolaIntersectionLeftOnly yd (xp1, yp1) (xp2, yp2) =
  case parabolaIntersection yd (xp1, yp1) (xp2, yp2) of
    Just (x, _) -> Just x
    Nothing -> Nothing




-- solving a 2nd degree equation
-- a x^2 + b x + c = 0
solve2 : Float -> Float -> Float -> Maybe (Float, Float)
solve2 a b c =
  let d2 = b * b - 4 * a * c
  in if d2 < 0
     then Nothing
     else let d = sqrt d2
              x1 = ((-1) * b - d) / (2 * a)
              x2 = ((-1) * b + d) / (2 * a)
          in Just (x1, x2)



-- x1 x3 x2 x3
circumCircle : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float, Float)
circumCircle (x1, y1) (x2, y2) (x3, y3) =
  if (x1 == x3)
  then circumCircle (x1, y1) (x3, y3) (x2, y2)
  else  if (x2 == x3)
        then circumCircle (x3, y3) (x2, y2)  (x1, y1)
        else
          let a1 = (y1-y3)/(x1-x3)
              xe = (x1+x3)/2
              ye = (y1+y3)/2
              b1 = ye + xe/a1

              a2 = (y2-y3)/(x2-x3)
              xf = (x2+x3)/2
              yf = (y2+y3)/2
              b2 = yf + xf/a2

              xc = (b2-b1) / (1/a2 - 1/a1)
              yc = b1 - xc / a1
              r = sqrt ((x1 - xc) *  (x1 - xc) + (y1 - yc) * (y1 - yc))
          in (xc, yc, r)


orderByAngles : (Float, Float) -> List (Float, Float) -> List (Float, Float)
orderByAngles (xc, yc) pts =
  let eps = 1e-5
      proc (x, y) =
        let dx = x - xc
            dy = y - yc
            angle = if abs dx > eps
                    then  let ta = dy / dx
                              a = atan ta
                              a1 = if dx > 0 then a else a + pi
                          in a1
                    else  let dx1 = dy
                              dy1 = -dx
                              cta = dx1 / dy1
                              a = atan cta
                              a1 = if dx1 > 0 then a else a + pi
                          in a1
            angle1 =  if angle >= 0
                      then  if angle <= 2 * pi
                            then angle
                            else angle - 2 * pi
                      else  angle + 2 * pi
        in (angle1, (xc, yc))
      pts1 = L.map second <| L.sortBy first <| L.map proc pts
  in pts1



type BoxSide =
  BoxSideLeft
  |BoxSideTop
  |BoxSideRight
  |BoxSideBottom

nextBoxSide s =
  case s of
    BoxSideLeft -> BoxSideTop
    BoxSideTop -> BoxSideRight
    BoxSideRight -> BoxSideBottom
    BoxSideBottom -> BoxSideLeft

previousBoxSide s =
  case s of
    BoxSideLeft -> BoxSideBottom
    BoxSideBottom -> BoxSideRight
    BoxSideRight -> BoxSideTop
    BoxSideTop -> BoxSideLeft


type Box = Box Float Float Float Float

-- x(t) = x1 (1-t) + x2 t  => x(t) = x1 + (x2-x1) t  => t = (x-x1)/(x2-x1)
-- y(t) = y1 (1-t) + y2 t  => y(t) = y1 + (y2-y1) t  => t = (y-y1)/(y2-y1)

type BoxIntersection =
  NoIntersection
  |Intersection BoxSide (Float, Float)

isInsideBox (Box xtl ytl xbr ybr) (x, y) =
    xtl <= x && x <= xbr && ybr <= y && y <= ytl

isOutsideBox b p = not (isInsideBox b p)

--bothInsideBox (Box xtl ytl xbr ybr) (x1, y1) (x2)


linearPrametric (x1, y1) (x2, y2) t =
    (x1 + (x2-x1) * t, y1 + (y2-y1) * t)


intersectSide (Box xtl ytl xbr ybr) side p1 p2 =
  let (x1, y1) = p1
      (x2, y2) = p2
      xFormula x = (x-x1)/(x2-x1)
      yFormula y = (y-y1)/(y2-y1)
  in case side of
        BoxSideLeft ->
          let t = xFormula xtl
          in  if 0 <= t && t <= 1
              then  let (xi, yi) = linearPrametric p1 p2 t
                    in  if ybr <= yi && yi <= ytl
                        then Intersection side (xtl, yi)
                        else NoIntersection
              else NoIntersection
        BoxSideTop ->
          let t = yFormula ytl
          in  if 0 <= t && t <= 1
              then  let (xi, yi) = linearPrametric p1 p2 t
                    in  if xtl <= xi && xi <= xbr
                        then Intersection side (xi, ytl)
                        else NoIntersection
              else NoIntersection
        BoxSideRight ->
          let t = xFormula xbr
          in  if 0 <= t && t <= 1
              then  let (xi, yi) = linearPrametric p1 p2 t
                    in  if ybr <= yi && yi <= ytl
                        then Intersection side (xbr, yi)
                        else NoIntersection
              else NoIntersection
        BoxSideBottom ->
          let t = yFormula ybr
          in  if 0 <= t && t <= 1
              then  let (xi, yi) = linearPrametric p1 p2 t
                    in  if xtl <= xi && xi <= xbr
                        then Intersection side (xi, ybr)
                        else NoIntersection
              else NoIntersection



intersectBox (Box xl yl xr yr as box) p1 p2 =
  let eps = 1e-6
      (x1, y1) = p1
      (x2, y2) = p2
      sides =
        if (abs (x1 - x2) > eps)
        then  if abs (y1 - y2) > eps
              then [BoxSideLeft, BoxSideTop, BoxSideRight, BoxSideBottom]
              else [BoxSideLeft, BoxSideRight]
        else [BoxSideTop, BoxSideBottom]
      proc side r =
        case r of
          NoIntersection -> intersectSide box side p1 p2
          _ -> r
  in  if isInsideBox box p1
      then  if isInsideBox box p2
            then NoIntersection
            else L.foldl proc NoIntersection sides
      else NoIntersection












