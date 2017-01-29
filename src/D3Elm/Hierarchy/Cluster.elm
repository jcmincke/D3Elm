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
    clusterSeparation = \na nb -> 1
    , clusterDx = 1.0
    , clusterDy = 1.0
    , clusterNodeSize = False
  }


mean : Dict Int Float -> List (Node d) -> Float
mean valueDict l =
  let proc n (acc, nb) =
        case D.get (getIndex n) valueDict of
          Just v -> (acc + v, nb+1.0)
          Nothing -> (acc, nb)
      (s, nb) = foldl proc (0.0, 1.0) l
  in s/nb


max : Dict Int Float -> Float -> List (Node d) -> Float
max valueDict minValue l =
  let proc n acc =
        case D.get (getIndex n) valueDict of
          Just v -> if acc < v then v else acc
          Nothing -> acc
  in foldl proc minValue l

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


cluster config root =
  let proc (xdic, ydic, previousNode, currentX) node =
        let i = getIndex node
        in  case getChildren node of
            [] ->
              case previousNode of
                Just pn ->  let currentX1 = currentX + config.clusterSeparation node pn
                            in (D.insert i currentX1 xdic, D.insert i 0.0 ydic, Just node, currentX1)
                Nothing ->  (D.insert i 0.0 xdic, D.insert i 0.0 ydic, Just node, currentX)
            children ->
              let x = mean xdic children
                  y = max ydic 0.0 children
              in (D.insert i x xdic, D.insert i y ydic, previousNode, currentX)
      (xdic, ydic, previousNode, currentX) = foldPostOrder proc (D.empty, D.empty, Nothing, 0.0)
      left = leafLeft root
      right = leafRight root

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


