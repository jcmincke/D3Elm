module Pie exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)


import ElmViz.Path.Path exposing (..)
import ElmViz.Shapes.Sector exposing (..)
import ElmViz.Shapes.Pie exposing (..)

main = html

data = [1,2,2,3,7,5]
--data = [1,1, 1, 1]

pieSpec : PieSpec Int
pieSpec = {
    getValue = toFloat
    , startAngle = 0
    , endAngle = 2 * pi
    , padAngle = 0.03
  }

sectorsPaths : PieSpec Int -> List (Html.Html msg)
sectorsPaths pieSpec =
  let pieResults = pie pieSpec data
      refSectorSpec = {sectorSpec0 | innerRadius = 80,  outerRadius = 100}
      proc res =
        let sectorSpec = {refSectorSpec | startAngle = res.startAngle, endAngle = res.endAngle, padAngle = res.padAngle}
            st = sector sectorSpec
        in S.path [SA.d st] []
  in L.map proc pieResults


html : Html.Html msg
html =
    svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)",  SA.stroke "green", SA.strokeWidth "1", SA.fill "blue"]
              (sectorsPaths pieSpec)
--               [text_ [SA.x "10", SA.y "20"] [text t]]

          ]










