module D3Elm.Geo.Projection.Gnomonic exposing (..)

import Basics as B exposing (..)

import D3Elm.Geo.Math as Math exposing (..)




gnomonic (x, y) =
  let cy = cos y
      k = cos x  * cy
  in (cy * sin x  / k, sin y  / k)

