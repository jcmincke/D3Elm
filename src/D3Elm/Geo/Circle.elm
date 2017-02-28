module D3Elm.Geo.Circle exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)

import GeoJson exposing (..)

import D3Elm.Geo.Common exposing (..)
import D3Elm.Geo.Math as Math exposing (..)
import D3Elm.Geo.Rotation as R exposing (..)


type alias CircleConf = {
  radiusAngle : Float
  , nbSteps : Int
  }


circle conf =
  let phi = pi/2 - radius
      lambdas  = mkList (-pi) pi [] conf.nbSteps
      rot = R.rotate 0 (pi/2) 0
      parallel = closingMap (\lambda -> rot (lambda, phi)) lambdas
  in parallel



















