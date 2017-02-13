module D3Elm.Voronoi.Common exposing (..)


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
                  (PointEvent _, CircleEvent _ _ _ _ _) -> L.reverse acc ++ (h::evt::r)
                  (CircleEvent _ _ _ _ _, PointEvent _) -> L.reverse acc ++ (evt::evts)
                  (CircleEvent _ _ _ _ _ as ce1, CircleEvent _ _ _ _ _ as ce2) ->
                    if ce1 == ce2
                    then evts0
                    else L.reverse acc ++ (h::evt::r)
  in go [] evts0


isMiddleArc _ s a b c =
  let (Site _ ((xa, ya) as pa)) = a
      (Site _ ((xb, yb) as pb)) = b
      (Site _ ((xc, yc) as pc)) = c
      (Site _ (x, y)) = s
      yai = parabola y pa x
      ybi = parabola y pb x
      yci = parabola y pc x
  in  if a == c
      then  if ybi <= yai
            then ArcFound (x, ybi)
            else  if xb < x
                  then OnRight
                  else OnLeft
      else  if x < xa
            then OnLeft
            else if x > xc
                 then OnRight
                 else  if ybi <= yai && ybi <= yci
                       then ArcFound (x, ybi)
                       else  if yai < ybi && yai < yci
                             then OnLeft
                             else OnRight

isBoundaryArc _ s side a b =
  case side of
    OpenOnRight ->
      let (Site _ pa) = a
          (Site _ ((xb, _) as pb)) = b
          (Site _ (x, y)) = s
          yai = parabola y pa x
          ybi = parabola y pb x
      in  if ybi <= yai
          then ArcFound (x, ybi)
          else OnLeft
    OpenOnLeft ->
      let (Site _ pa) = a
          (Site _ ((xb, _) as pb)) = b
          (Site _ (x, y)) = s
          yai = parabola y pa x
          ybi = parabola y pb x
      in  if yai <= ybi
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




circumCircle2 (x1, y1) (x2, y2) (x3, y3) =
  let nx = (x3*x3 - x2*x2 + y3*y3 - y2*y2) / (2 * (y3 - y2)) - (x2*x2 - x1*x1 + y2*y2 - y1*y1) / (2 * (y2 - y1))
      dx = ((x2 - x1) / (y2 - y1)) - ((x3 - x2) / (y3 - y2))
      xc = nx / dx
      yc = ((-1) * (x2 - x1) * xc / (y2 - y1)) + ((x2*x2 - x1*x1 + y2*y2 - y1*y1) / (2 * (y2 - y1)))
      r = sqrt ((x1 - xc) *  (x1 - xc) + (y1 - yc) * (y1 - yc))
  in (xc, yc, r)







