
module D3Elm.Geo.Clip.Circle exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)

import D3Elm.Common exposing (..)
import D3Elm.Geo.Cartesian exposing (..)
import D3Elm.Geo.Math exposing (..)

type PtLoc =
  InsideCircle
  |OusideCircle
  |OnCircle


circlePolygonIntersection : [(Float, Float)] -> Float -> [(Float, Float)]
circlePolygonIntersection polygon radiusAngle =
  let go pts =
    case pts of
      [] -> []
      (a::b::r) ->
        case circleArcIntersection a b radiusAngle of
          (_::t) -> t ++ go (b::r)
          [] -> [] -- should never happen
  in  case polygon of
      [] -> polygon
      [_] -> polygon
      [_,_] -> polygon
      -- a real polygon
      (a::_::_) -> cleanup (a::go polygon)


cleanup : [((Float, Float), PtLoc)] -> [(Float, Float)]
cleanup pts =
  let go pts =
    case pts of
      ((a, OusideCircle)::r) -> go r
      ((a, _)::r) -> a::go r
  in go pts



circleArcIntersection : (Float, Float) -> (Float, Float) -> Float -> [((Float, Float), PtLoc)]
circleArcIntersection ((lambda1, phi1) as a) ((lambda2, phi2) as b) radiusAngle =
  let xc = cos radiusAngle
      ((xa, ya, za) as pa) = cartesian a
      ((xb, yb, zb) as pb) = cartesian b
  in  if xa >= xc && xb >= xc
      then  -- both points inside the circle
            [(a, InsideCircle), (b, InsideCircle)]
      else  if xa >= xc && xb < xc
            then  case oneIntersection a b radiusAngle of
                  Just i -> [(a, InsideCircle), i, (b, OusideCircle)]
                  Nothing -> [(a, InsideCircle), (b, OusideCircle)]  -- should never happen
            else  if xa < xc && xb >= xc
                  then case oneIntersection b a radiusAngle of
                  Just i -> [(a, OusideCircle), i, (b, InsideCircle)]
                  Nothing -> [(a, OusideCircle), (b, InsideCircle)]  -- should never happen
                  else [(a, OusideCircle)] ++ twoIntersections a b radiusAngle ++ [(b, OusideCircle)]



-- The circle is crossed only once. That means we assume that a is inside the circle and b is outside
-- It is up to the caller to correctly order a and b.
oneIntersection ((lambda1, phi1) as a) ((lambda2, phi2) as b) radiusAngle =
  let -- compute cartesian coordinates
      ((xa, ya, za) as pa) = cartesian a
      ((xb, yb, zb) as pb) = cartesian b
      -- normal of the plane defined by the arc
      -- the plan is defined by the equation: nx x + ny y + nz * z = 0
      (nx, ny, nz) = crossProduct pa pb

      -- x coordinate of the plan defined by the circle
      xc = cos radiusAngle

      -- intersection of the circle plane and the arc plane.
      icm = lineCircleIntersection ny nz (-1 * nx * xc) radiusAngle
  case icm of
    Just ((yc1, zc1), (yc2, zc2)) ->
      let i1 = spherical (xc, yc1, zc1)
          i2 = spherical (xc, yc2, zc2)
          i = findIntersectionOnArc a b i1 i2
      in Just (i, OnCircle)
    Nothing -> Nothing   -- should never happen


-- Both points a and b are outside the circle.
-- The circle may then be not crossed [0, 1, 2] times.
twoIntersections ((lambda1, phi1) as a) ((lambda2, phi2) as b) radiusAngle =
  let -- compute cartesian coordinates
      ((xa, ya, za) as pa) = cartesian a
      ((xb, yb, zb) as pb) = cartesian b
      -- normal of the plane defined by the arc
      -- the plan is defined by the equation: nx x + ny y + nz * z = 0
      (nx, ny, nz) = crossProduct pa pb

      -- x coordinate of the plan defined by the circle
      xc = cos radiusAngle

      -- intersection of the circle plane and the arc plane.
      icm = lineCircleIntersection ny nz (-1 * nx * xc) radiusAngle
  case icm of
    Just (((yc1, zc1) as ic1), ((yc2, zc2) as ic2)) ->
      if ic1 /= ic2
      then
        let i1 = spherical (xc, yc1, zc1)
            i2 = spherical (xc, yc2, zc2)
            (oi1, oi2) = orderIntersectionsOnArc a b i1 i2
        in [(oi1, OnCircle), (oi2, OnCircle)]
      else  -- the arc is tangent, discard the intersection point
            [(a, OusideCircle), (b, OusideCircle)]
    Nothing -> []


findIntersectionOnArc ((lambda1, phi1) as a) ((lambda2, phi2) as b) ((lambdaI1, phiI1) as i1) ((lambdaI2, phiI2) as i2) =
  let aTob = orthodromicLength a b
      aToi1 = orthodromicLength a i1
      aToi2 = orthodromicLength a i2
      i1Tob = orthodromicLength i1 b
      i2Tob = orthodromicLength i2 b
  in  if abs (aToi1 + i1Tob) < abs (aToi2 + i2Tob)
      then i1
      else i2

orderIntersectionsOnArc ((lambda1, phi1) as a) ((lambda2, phi2) as b) ((lambdaI1, phiI1) as i1) ((lambdaI2, phiI2) as i2) =
  let aToi1 = orthodromicLength a i1
      aToi2 = orthodromicLength a i2
      if aToi1 < aToi2
      then (i1, i2)
      else (i2, i1)


-- true if the point is inside the circle.
-- Compare the x-cartesian coordinate of the point and of the circle plane

isInsideCircle (lambda1, phi1) radiusAngle =
  let xc = cos radiusAngle



-- intersection of a circle centered on (0,0) with radius r
-- and a line : a y + b z = c

lineCircleIntersection a b c r =
  let a1 = a*a + b*b
      b1 = -2 * a * c
      c1 = c*c - r*r*b*b
      zf y = (c - a * y)/b
  in case solve2 a1 b1 c1 of
      Nothing -> Nothing
      Just (y1, y2) -> Just ((y1, zf y1), (y2, zf y2))



