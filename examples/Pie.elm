module Pie exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)


import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Sector exposing (..)
import D3Elm.Shapes.Pie exposing (..)

main = html

data = [1,2,2,3,7,5]

pieSpec : PieSpec Int
pieSpec = {
    getValue = toFloat
    , startAngle = 0
    , endAngle = 2 * pi
    , padAngle = 0
  }

--sectorsPaths : PieSpec d -> []
sectorsPaths pieSpec =
  let pieResults = pie pieSpec data
      refSectorSpec = {sectorSpec0 | innerRadius = 80,  outerRadius = 100}
      proc res =
        let sectorSpec = {refSectorSpec | startAngle = res.startAngle, endAngle = res.endAngle}
            st = sector sectorSpec
        in S.path [SA.d st] []
  in L.map proc pieResults




sectorSpec0 : SectorSpec
sectorSpec0 = {
    innerRadius = 0
    , outerRadius = 1
    , cornerRadius = 0
    , startAngle = 0
    , endAngle = pi/6
    , padAngle = 0
  }

html : Html.Html msg
html =
    svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)",  SA.stroke "green", SA.strokeWidth "2", SA.fill "none"]
              (sectorsPaths pieSpec)
          ]










