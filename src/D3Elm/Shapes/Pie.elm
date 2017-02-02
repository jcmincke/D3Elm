module D3Elm.Shapes.Pie exposing (..)

import Basics exposing (..)
import List as L exposing (..)

import D3Elm.Path.Path exposing (..)


type alias PieSpec d = {
    getValue : d -> Float
    , startAngle : Float
    , endAngle : Float
    , padAngle : Float
  }



pieSpec0 : PieSpec d
pieSpec0 = {
    getValue = \_ -> 1
    , startAngle = 0
    , endAngle = pi
    , padAngle = 0
  }

type alias PieResults d = {
    data : d
    , startAngle : Float
    , endAngle : Float
    , padAngle : Float
  }

zip: List a -> List b -> List (a,b)
zip la lb =
  case (la, lb) of
    ([], _) -> []
    (_, []) -> []
    (ha::ta, hb::tb) -> (ha, hb) :: zip ta tb

pie : PieSpec d -> List d -> List (PieResults d)
pie spec data =
  case data of
    [] -> []
    _ ->
      let totalValue = L.foldl (+) 0 <| map spec.getValue data
          totalAngle = spec.endAngle - spec.startAngle - spec.padAngle * (toFloat (L.length data))
          anglesProc d =
            let v = spec.getValue d
            in v * totalAngle / totalValue
          sectorAngles = L.map anglesProc data
          cumAnglesProc acc last angles =
            case angles of
              [] -> reverse acc
              (h::t) -> cumAnglesProc ((last, last + h)::acc) (last + h) t
          cumAngles = cumAnglesProc [] spec.startAngle sectorAngles
          resProc (d, (sa, ea)) = {data = d, startAngle = sa, endAngle = ea, padAngle = spec.padAngle / 2}
      in L.map resProc <| zip data cumAngles









