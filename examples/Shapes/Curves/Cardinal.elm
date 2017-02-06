module Shapes.Curves.Cardinal exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)


import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Curves.Cardinal exposing (..)


main = mainHtml

datab = [
  (0,8)
  , (1,-1)
  , (2,2)
  , (3,1)
  , (4,-3)
  , (5, 8)
  ]

data = L.map (\(x,y) -> (x*100, y*20)) datab

points data =
  let proc (x,y) = S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "5"] []
  in L.map proc data

--  <circle cx="100" cy="10" r="2" fill="blue"/>
mainHtml : Html.Html msg
mainHtml =
  let tension = 0.4
      dstr = (cardinal tension data path0).thePath
  in svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)"]
              (S.path [SA.d dstr, SA.stroke "green", SA.strokeWidth "2", SA.fill "none"]
                       []

            :: points data
            )

          ]











