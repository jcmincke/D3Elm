module TestD3Elm.D3Elm.Geo.Clip.Circle exposing (..)

import Debug as D exposing (..)
import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (floatRange, tuple, tuple5, constant)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)
import Round as R exposing (..)


import D3Elm.Common exposing (..)
import D3Elm.Geo.Math exposing (..)
import D3Elm.Geo.Cartesian exposing (..)
import D3Elm.Geo.Clip.Circle exposing (..)
import D3Elm.Geo.Length exposing (..)


eps = 1e-3

all : Test
all =
    describe "Circle Clipping "
        [ --testCircleClip
          test "" testCircleClip1
        ]

{-}
testRotate  =
  let expectation (deltaLambda, deltaPhi, deltaGamma, lambda, phi) =
        let c = round3 (cartesian (lambda, phi))
            r = rotate deltaLambda deltaPhi deltaGamma (lambda, phi)
            r1 = invRotate deltaLambda deltaPhi deltaGamma r
            cr1 =  round3 (cartesian r1)
        in Expect.equal cr1 c

      dl = floatRange (-tau) tau
      dp = floatRange (-tau) tau
      dg = floatRange (-tau) tau
      l = floatRange (-tau) tau
      p = floatRange 0 tau
      fuzzer = tuple5 (dl, dp, dg, l, p)
  in fuzz fuzzer "Rotate" expectation
-}

testCircleClip =
  let minima = 0.1
      maxima = 1.5
      dl1 = floatRange minima maxima
      --dl1 = floatRange (-pi+epsilon) pi
      dp1 = floatRange minima maxima
      dl2 = floatRange minima maxima
      dp2 = floatRange minima maxima
      dra =  constant (pi/3) --floatRange epsilon (pi/2 - epsilon) --
      fuzzer = tuple5 (dl1, dp1, dl2, dp2, dra)
  in fuzz fuzzer "Circle Clipping" (\(l1, p1, l2, p2, ra) -> circleClipExpectation (l1, p1) (l2,p2) ra)

-- 1.211386428725057,0.10428717230081468,0.7797823415972933,0.791041010696051,1.0471975511965976
-- 1.0443402828083541,0.1,1.0337516038915073,0.30013783184118653,1.0471975511965976
testCircleClip1 () =
  let dl1 =  1.0443402828083541 --floatRange (-pi) pi
      dp1 =  0.1 --floatRange (-pi) pi
      dl2 =  1.0337516038915073 --floatRange (-pi) pi
     -- dl2 =  1.0443402828083541 --floatRange (-pi) pi
      dp2 =  0.30013783184118653 --floatRange (-pi) pi
      dra =  (pi/3) --floatRange epsilon (pi/2 - epsilon)
  in circleClipExpectation (dl1, dp1) (dl2,dp2) dra

circleClipExpectation a b radiusAngle =
  let clipped = D.log "res:" <| circleArcIntersection a b radiusAngle
      clipped1 = L.map first clipped
      l = D.log "length seq= "<| olength clipped1
      b1 = D.log "B1= " <| pointsOnSameArc clipped1
      b2 = pointsInrightOrderOnArc clipped1
      b3 = intersectionPointsOnCircle clipped1 radiusAngle
      d = orthodromicLength a b
  in Expect.true "circleClipExpectation" (if d > 1e-2 then b1 else True) --(b1 && b2 && b3)

olength pts =
  let go pts =
    case pts of
      [] -> []
      [a] -> []
      (a::b::r) -> orthodromicLength a b :: go (b::r)
  in go pts

intersectionPointsOnCircle pts radiusAngle =
  case pts of
    [] -> True
    [a] -> True
    [a,b] -> True
    [a,b,c] -> pointOnCircle b radiusAngle
    [a,b,c,d] -> pointOnCircle b radiusAngle && pointOnCircle c radiusAngle
    _ -> False

pointOnCircle a radiusAngle =
  let circleEq (x, y, z) =
        -- equation of the clip circle
        let radius = sin radiusAngle
            xc = cos radiusAngle
        in (x-xc)*(x-xc) + y * y + z * z - radius * radius
      ca = cartesian a
  in abs (circleEq ca) < eps

onSameArc a b c =
  let xa = cartesian a
      xb = cartesian b
      xc = cartesian c
      cpab = D.log "X prod cpab" <| normalizeVector <| crossProduct3D xa xb
      cpbc = D.log "X prod cpbc" <| normalizeVector <| crossProduct3D xb xc
      d = D.log "length" <| cartesianLength cpab cpbc
      bool = D.log "bool" (d < eps)
  in bool

pointsOnSameArc pts =
  let l = D.log "pointsOnSameArc:" pts
  in
  case pts of
    [] -> True
    [a] -> True
    [a,b] -> True
    [a,b,c] -> onSameArc a b c
    [a,b,c,d] -> D.log "pointsOnSameArc: " <| onSameArc a b c && onSameArc b c d
    _ -> False


pointsInrightOrderOnArc pts =
  case pts of
    [] -> True
    [a] -> True
    [a,b] -> True
    [a,b,c] ->
      let lac = orthodromicLength a c
          lab = orthodromicLength a b
          lbc = orthodromicLength b c
      in abs (lac - lab - lbc) < eps
    [a,b,c,d] ->
      let lad = orthodromicLength a d
          lab = orthodromicLength a b
          lbc = orthodromicLength b c
          lcd = orthodromicLength c d
      in abs (lad - lab - lbc - lcd) < eps
    _ -> False


normalizeVector (nx, ny, nz) =
  let d = sqrt (nx * nx + ny * ny + nz * nz)
  in (nx/d, ny/d, nz/d)



