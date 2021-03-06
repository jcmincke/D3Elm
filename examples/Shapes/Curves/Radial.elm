module Shapes.Curves.Radial exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)

import Basics exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Path.Path exposing (..)
import ElmViz.Shapes.Curves.Radial exposing (..)


main = mainHtml

datab = [
  (0,8)
  , (45,1)
  , (90,2)
  , (135,1)
  , (180, 3)
  , (225, 8)
  , (270, 10)
  , (315, 7)
  , (360, 8)
  ]



data =
  let angles = mkList 0 360
  in L.map (\a -> let ra = toFloat a * pi / 180
                  in (50 + 10 * sin (10 * ra)  , ra)) angles

points data =
  let proc (x,y) = S.circle [SA.cx (toString x), SA.cy (toString y), SA.r "5"] []
  in L.map proc data

mainHtml : Html.Html msg
mainHtml =
  let dstr = (radial data path0).thePath
  in svg  [ width "1000", height "600", viewBox "0 0 1000 600" ]
          [ g [SA.transform "translate(200, 200)"]
              (S.path [SA.d dstr, SA.stroke "green", SA.strokeWidth "1", SA.fill "none"]
                       []
              ::[]
          --  :: points data
            )

          ]











