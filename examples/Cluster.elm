module Cluster exposing (main)

import Html
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Dict as D exposing (..)
import List as L exposing (..)

import Logger as Logger exposing (..)

import D3Elm.Hierarchy.Tree.Tree exposing (..)
import D3Elm.Hierarchy.Cluster exposing (..)

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

main = clusterHtml
--H.text "hello"

clusterConfig ={defaultClusterConfig | clusterDx = 500, clusterDy = 500} --, clusterNodeSize = True}
--clusterConfig ={defaultClusterConfig | clusterNodeSize = True}

xyDic = cluster clusterConfig node

labels =
  let proc (i, (xn,yn)) = text_ [ SA.x (toString (yn )), SA.y (toString (xn ))] [text (toString i)]
  in L.map proc <| D.toList xyDic


edges =
  let links = getLinks node
      proc (np, nc) =
        let (xp0, yp0) = getCoord xyDic np
            (xc0, yc0) = getCoord xyDic nc
            (xp, yp) = (xp0, yp0)
            (xc, yc) = (xc0, yc0)
            ds =  "M " ++ toString yc ++ "," ++ toString xc ++
                  " C " ++ toString (yp+100) ++ " " ++ toString xc ++
                  " , " ++ toString (yp+100) ++ " " ++ toString xp ++
                  ", " ++ toString yp ++ " " ++ toString xp
            _ = log "" ds
        in S.path [ SA.d ds, SA.style "fill: none; stroke: #555; stroke-width: 1.5px; stroke-opacity: 0.4"] []
                        --circle [ cx (toString (x*200)), cy (toString (y*200)), r "5" ] []
  in L.map proc links

clusterHtml : Html.Html msg
clusterHtml =
  let _ = log "" xyDic
  in  svg
        [ width "1000", height "600", viewBox "0 0 1000 600" ]
        (
            labels
            ++  edges
          )
