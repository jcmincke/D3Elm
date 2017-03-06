module Treemap exposing (main)

--import Html as H exposing (Html, button, div, text)
import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)

import Logger as Logger exposing (..)
--import Svg as S exposing (..)

import ElmViz.Hierarchy.Tree.Tree exposing (..)
import ElmViz.Hierarchy.Treemap.Treemap exposing (..)

loggerConfig : Logger.Config a
loggerConfig =
    Logger.defaultConfig  Logger.Debug

log : String -> a -> a
log =
    Logger.log loggerConfig Logger.Debug


mkNodeInfo d h = NodeInfo d 0 h

node = let
        c = Leaf 1 (mkNodeInfo "C" 0)
        e = Leaf 2 (mkNodeInfo "E" 0)
        d = Node 3 (mkNodeInfo "D" 1) [c,e]
        a = Leaf 4 (mkNodeInfo "A" 0)
        b = Node 5 (mkNodeInfo "B" 2) [a,d]
        h = Leaf 6 (mkNodeInfo "H" 0)
        i = Node 7 (mkNodeInfo "I" 1) [h]
        g = Node 8 (mkNodeInfo "G" 2) [i]
        f = Node 9 (mkNodeInfo "F" 3) [b,g]
    in f

main = treemap


boxHtml ratio boxDic =
  let proc (_, ((x1_, y1_), (x2_, y2_))) =
        let x1 = x1_ * ratio
            x2 = x2_ * ratio
            y1 = y1_ * ratio
            y2 = y2_ * ratio
        in S.rect [ SA.x (toString x1), SA.y (toString y1)
                    , SA.width (toString (x2-x1)), SA.height (toString (y2-y1))
                    , SA.style "fill: none;stroke:pink;stroke-width:1"] []
  in L.map proc <| D.toList boxDic

--<svg width="400" height="110">
--  <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
--</svg>

treemap : Html.Html msg
treemap =
  let cost n = toFloat (getIndex n)
      boxDic = sliceAndDice 0.99 cost node
  in  svg
        [ width "1000", height "600", viewBox "0 0 1000 600" ]
        (
            boxHtml 300 boxDic
          )










