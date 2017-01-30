module D3Elm.Hierarchy.Cluster exposing (..)

import Lazy as L
import List exposing (append, foldl, reverse, maximum, map, head, sortWith)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)

import D3Elm.Hierarchy.Tree.Tree exposing (..)


type alias ClusterConfig d = {
  clusterSeparation : Node d -> Node d -> Float
  , clusterDx : Float
  , clusterDy : Float
  , clusterNodeSize : Bool
}


defaultClusterConfig : ClusterConfig d
defaultClusterConfig = {
    clusterSeparation = \na nb -> 1.0
    , clusterDx = 1.0
    , clusterDy = 1.0
    , clusterNodeSize = False
  }


meanX : Dict Int (Float, Float) -> List (Node d) -> Float
meanX valueDict l =
  let proc n (acc, nb) =
        case D.get (getIndex n) valueDict of
          Just (v, _) -> (acc + v, nb+1.0)
          Nothing -> (acc, nb)
      (s, nb) = foldl proc (0.0, 1.0) l
  in s/nb


maxY : Dict Int (Float, Float) -> Float -> List (Node d) -> Float
maxY valueDict minValue l =
  let proc n acc =
        case D.get (getIndex n) valueDict of
          Just (_, v) -> if acc < v then v else acc
          Nothing -> acc
  in 1 + foldl proc minValue l

leafLeft : Node d -> Node d
leafLeft node =
  case node of
    Leaf _ _ -> node
    Node _ _ (c::_) -> leafLeft c
    Node _ _ [] -> node

leafRight : Node d -> Node d
leafRight node =
  case node of
    Leaf _ _ -> node
    Node _ _ cs ->
      case reverse cs of
        (c::_) -> leafRight c
        [] -> node

getCoord xydic node = withDefault (0.0, 0.0) <| D.get (getIndex node) xydic

cluster config root =
  let proc (xydic, previousNode, currentX) node =
        let i = getIndex node
        in  case getChildren node of
            [] ->
              case previousNode of
                Just pn ->  let currentX1 = currentX + config.clusterSeparation node pn
                            in (D.insert i (currentX1, 0.0) xydic, Just node, currentX1)
                Nothing ->  (D.insert i (0.0, 0.0) xydic, Just node, currentX)
            children ->
              let x = meanX xydic children
                  y = maxY xydic 0.0 children
              in (D.insert i (x, y) xydic, previousNode, currentX)
      (xydic, previousNode, currentX) = foldPostOrder proc (D.empty, Nothing, 0.0) root
      leftNode = leafLeft root
      rightNode = leafRight root
      (x0b, _) = getCoord xydic leftNode
      x0 = x0b - (config.clusterSeparation leftNode rightNode) / 2.0
      (x1b, _) = getCoord xydic rightNode
      x1 = x1b + (config.clusterSeparation leftNode rightNode) / 2.0

      (xRoot, yRoot) = getCoord xydic root

      proc2 xydic node =
        let i = getIndex node
            (xNode, yNode) = getCoord xydic  node
        in  if config.clusterNodeSize
            then  let x = (xNode - xRoot) * config.clusterDx
                      y = (yNode - yRoot) * config.clusterDy
                  in D.insert i (x, y) xydic
            else  let x = (xNode - x0) / (x1 - x0) * config.clusterDx
                      y = (1.0 - yNode / yRoot) * config.clusterDy
                  in D.insert i (x, y) xydic
  in foldPostOrder proc2 xydic root


{-
 function cluster(root) {
    var previousNode,
        x = 0;

    // First walk, computing the initial x & y values.
    root.eachAfter(function(node) {
      var children = node.children;
      if (children) {
        node.x = meanX(children);
        node.y = maxY(children);
      } else {
        node.x = previousNode ? x += separation(node, previousNode) : 0;
        node.y = 0;
        previousNode = node;
      }
    });

    var left = leafLeft(root),
        right = leafRight(root),
        x0 = left.x - separation(left, right) / 2,
        x1 = right.x + separation(right, left) / 2;

    // Second walk, normalizing x & y to the desired size.
    return root.eachAfter(nodeSize ? function(node) {
      node.x = (node.x - root.x) * dx;
      node.y = (root.y - node.y) * dy;
    } : function(node) {
      node.x = (node.x - x0) / (x1 - x0) * dx;
      node.y = (1 - (root.y ? node.y / root.y : 1)) * dy;
    });
}
-}


