module D3Elm.Voronoi.Common exposing (..)


import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)
import Basics exposing (..)


type alias Point = {
    x : Float
    , y : Float
  }

type Site = Site Int (Float, Float)


type Event site =
  PointEvent site
  | CircleEvent site site site (Float, Float) Float



type ArcPred =
  ArcFound
  |OnLeft
  |OnRight

type OpenOn = OpenOnRight | OpenOnLeft


insertEvent : (Event site -> Event site -> Order) -> Event site -> List (Event site) -> List (Event site)
insertEvent comparer evt evts0 =
  let go acc evts =
    case evts of
      [] -> [evt]
      (h::r) ->
        case comparer evt h of
          LT -> L.reverse acc ++ (evt::evts)
          GT -> go (h::acc) r
          EQ -> case (evt, h) of
                  (PointEvent _, PointEvent _) -> L.reverse acc ++ (evt::evts)
                  (PointEvent _, CircleEvent _ _ _ _ _) -> L.reverse acc ++ (h::evt::r)
                  (CircleEvent _ _ _ _ _, PointEvent _) -> L.reverse acc ++ (evt::evts)
                  (CircleEvent _ _ _ _ _ as ce1, CircleEvent _ _ _ _ _ as ce2) ->
                    if ce1 == ce2
                    then evts0
                    else L.reverse acc ++ (evt::evts)
  in go [] evts0


isMiddleArc yd s a b c =
  let (Site _ ((xa, ya) as pa)) = a
      (Site _ pb) = b
      (Site _ ((xc, yc) as pc)) = c
      (Site _ (x, y)) = s
      yai = parabola yd pa x
      ybi = parabola yd pb x
      yci = parabola yd pc x
  in if x < xa
     then OnLeft
     else if x > xc
          then OnRight
          else  if ybi <= yai && ybi <= yci
                then ArcFound
                else  if yai < ybi && yai < yci
                      then OnLeft
                      else OnRight

isBoundaryArc yd s side a b =
  case side of
    OpenOnRight ->
      let (Site _ pa) = a
          (Site _ ((xb, _) as pb)) = b
          (Site _ (x, y)) = s
          yai = parabola yd pa x
          ybi = parabola yd pb x
      in  if xb < x && ybi <= yai
          then ArcFound
          else OnLeft
    OpenOnLeft ->
      let (Site _ pa) = a
          (Site _ ((xb, _) as pb)) = b
          (Site _ (x, y)) = s
          yai = parabola yd pa x
          ybi = parabola yd pb x
      in  if x < xb && yai <= ybi
          then ArcFound
          else OnRight




-- parabola equation
parabola yd (xf, yf) x =
  let p = yf - yd
  in (x-xf)^2 / (2 * p) + yd + (yf + yd) / 2

-- finding intersection between 2 parabolas

parabolaIntersection yd (xp1, yp1) (xp2, yp2) =
  let p1 = yp1 - yd
      p2 = yp2 - yd
      a = p2 - p1
      b = 2 * (p1 * xp2 - p2 * xp1)
      c = p2 * xp1 * xp1 - p1 * xp2 * xp2 + p1 * p2 * (yp1 - yp2)
      rm = solve2 a b c
  in rm





-- solving a 2nd degree equation
-- a x^2 + b x + c = 0

solve2 a b c =
  let d2 = b * b - 4 * a * c
  in if d2 < 0
     then Nothing
     else let d = sqrt d2
              x1 = ((-1) * b - d) / (2 * a)
              x2 = ((-1) * b + d) / (2 * a)
          in Just (x1, x2)





circumCircle (x1, y1) (x2, y2) (x3, y3) =
  let nx = (x3*x3 - x2*x2 + y3*y3 - y2*y2) / (2 * (y3 - y2)) - (x2*x2 - x1*x1 + y2*y2 - y1*y1) / (2 * (y2 - y1))
      dx = (x2 - x1) / (y2 - y1) - (x3 - x2) / (y3 - y2)
      xc = nx / dx
      yc = -(x2 - x1) * xc / (y2 - y1) + (x2*x2 - x1*x1 + y2*y2 - y1*y1) / (2 * (y2 - y1))
      r = sqrt ((x1 - xc) *  (x1 - xc) + (y1 - yc) * (y1 - yc))
  in (xc, yc, r)
