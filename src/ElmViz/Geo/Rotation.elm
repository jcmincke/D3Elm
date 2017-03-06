module ElmViz.Geo.Rotation exposing (
  rotate,
  invRotate,
  genericNormalize
  )

import Basics as B exposing (..)

import ElmViz.Geo.Math as Math exposing (..)

asin1 = Math.asin

genericNormalize bottom angle =
  let d = floor (angle / tau)
      angle1 = angle - bottom * tau
  in if angle < bottom
     then angle + tau
     else if angle >= bottom + tau
          then angle - tau
          else angle

normalizePi = genericNormalize (-pi)
normalize0 = genericNormalize 0

-- !! radians
rotate : Float -> Float -> Float -> ((Float, Float) -> (Float, Float))
rotate deltaLambda deltaPhi deltaGamma =
  let normalize2 (l, p) = (normalizePi l, normalize0 p)
      proc =  if (deltaPhi, deltaGamma) /= (0,0)
              then ((forwardRotationLambda deltaLambda >> rotationPhiGamma deltaPhi deltaGamma) >> normalize2)
              else (forwardRotationLambda deltaLambda >> normalize2)
  in proc

invRotate : Float -> Float -> Float -> ((Float, Float) -> (Float, Float))
invRotate deltaLambda deltaPhi deltaGamma =
  let normalize2 (l, p) = (normalizePi l, normalize0 p)
  in  if (deltaPhi, deltaGamma) /= (0,0)
      then (invRotationPhiGamma deltaPhi deltaGamma >> forwardRotationLambda (-deltaLambda) >> normalize2)
      else (forwardRotationLambda (-deltaLambda) >> normalize2)

forwardRotationLambda : Float -> ((Float, Float) -> (Float, Float))
forwardRotationLambda deltaLambda (lambda, phi) =
  (lambda + deltaLambda, phi)

rotationPhiGamma : Float -> Float -> ((Float, Float) -> (Float, Float))
rotationPhiGamma deltaPhi deltaGamma =
  let cosDeltaPhi = cos deltaPhi
      sinDeltaPhi = sin deltaPhi
      cosDeltaGamma = cos deltaGamma
      sinDeltaGamma = sin deltaGamma
      proc (lambda, phi) =
        let cosPhi = cos phi
            x = cos lambda * cosPhi
            y = sin lambda * cosPhi
            z = sin phi
            k = z * cosDeltaPhi + x * sinDeltaPhi
            lambda1 = atan2 (y * cosDeltaGamma - k * sinDeltaGamma) (x * cosDeltaPhi - z * sinDeltaPhi)
            phi1 = asin1 (k * cosDeltaGamma + y * sinDeltaGamma)
        in (lambda1, phi1)
  in proc





invRotationPhiGamma : Float -> Float -> ((Float, Float) -> (Float, Float))
invRotationPhiGamma deltaPhi deltaGamma =
  let cosDeltaPhi = cos deltaPhi
      sinDeltaPhi = sin deltaPhi
      cosDeltaGamma = cos deltaGamma
      sinDeltaGamma = sin deltaGamma
      proc (lambda, phi) =
        let cosPhi = cos phi
            x = cos lambda * cosPhi
            y = sin lambda * cosPhi
            z = sin phi
            k = z * cosDeltaGamma - y * sinDeltaGamma
            lambda1 = atan2 (y * cosDeltaGamma + z * sinDeltaGamma) (x * cosDeltaPhi + k * sinDeltaPhi)
            phi1 = asin1 (k * cosDeltaPhi - x * sinDeltaPhi)
        in (lambda1, phi1)
  in proc


