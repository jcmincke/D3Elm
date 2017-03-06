module TestElmViz.ElmViz.Geo.Rotation exposing (..)

import Debug exposing (..)
import Basics exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (floatRange, tuple, tuple5, constant)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)
import Tuple exposing (..)
import Round as R exposing (..)


import ElmViz.Geo.Math exposing (..)
import ElmViz.Geo.Cartesian exposing (..)
import ElmViz.Geo.Rotation exposing (..)



all : Test
all =
    describe "Rotation "
        [ testNormalize
          , testRotate
        ]

normalizeRef bottom angle =
  if angle >= (bottom + tau)
  then normalizeRef bottom (angle - tau)
  else  if angle < bottom
        then normalizeRef bottom (angle + tau)
        else angle

round3 (x, y, z) =
  let p = 6
  in (R.round p x, R.round p y, R.round p z)


testNormalize  =
  let expectation (bottom, angle) =
        Expect.equal (genericNormalize bottom angle) (genericNormalize bottom angle)
      bottomFuzz = floatRange (-pi) pi
      angleFuzz = floatRange (-1000) 1000
      fuzzer = tuple (bottomFuzz, angleFuzz)
  in fuzz fuzzer "Normalize" expectation

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

