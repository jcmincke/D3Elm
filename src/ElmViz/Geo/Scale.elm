module ElmViz.Geo.Scale exposing (
  scale,
  translate
  )

import Basics as B exposing (..)



scale : Float -> Float -> ((Float, Float) -> (Float, Float))
scale sx sy (x, y) = (sx * x, sy * y)

translate : Float -> Float -> ((Float, Float) -> (Float, Float))
translate dx dy (x, y) = (x + dx, y + dy)


