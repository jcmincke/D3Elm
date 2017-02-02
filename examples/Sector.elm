module Sector exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)

import Logger as Logger exposing (..)
--import Svg as S exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Sector exposing (..)

loggerConfig : Logger.Config a
loggerConfig =
    Logger.defaultConfig  Logger.Debug

log : String -> a -> a
log =
    Logger.log loggerConfig Logger.Debug


main = sectorHtml


sectorSpec = {sectorSpec0 | innerRadius = 80,  outerRadius = 100, startAngle = 0, endAngle =  1 * pi + 0.1}

sectorHtml : Html.Html msg
sectorHtml =
  let t = sector sectorSpec
  in svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)"]
              [ S.path [SA.d t, SA.stroke "green", SA.strokeWidth "2", SA.fill "blue"]
                       []
              ]
            , text_ [SA.x "10", SA.y "20"] [text t]
             -- g [transform "translate(50, 50)"]
             --   [text t]
          ]

--  <g transform="translate(10, 50)">
--  <path d="M 0  0 L 100  0" stroke="green"
--  stroke-width="2" fill = "none"/>
--  </g>










