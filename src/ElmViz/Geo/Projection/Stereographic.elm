module ElmViz.Geo.Projection.Stereographic exposing (..)

import Basics as B exposing (..)

import ElmViz.Geo.Math as Math exposing (..)





stereographic(x, y) =
  let cy = cos y
      k = 1 + cos x * cy
  in (cy * sin x  / k, sin y  / k)
