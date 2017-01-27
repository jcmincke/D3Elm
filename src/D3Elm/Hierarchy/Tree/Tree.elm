module D3Elm.Hierarchy.Tree.Tree exposing (..)

import Lazy as L
import List exposing (append, foldl)

type alias NodeInfo data = {
  nodeData : data
  , nodeDepth : Int
  , nodeHeight : Int
}

type Node d =
  Node Int (L.Lazy (Maybe Node)) (NodeInfo d) (List (Node d))
  |Leaf Int (L.Lazy (Maybe Node)) (NodeInfo d)



foldBreadthFirst : (acc -> Int -> d -> acc) -> acc -> Node d -> acc
foldBreadthFirst f acc node =
  let proc acc nodes =
    case nodes of
      [] -> acc
      (Leaf i _ info :: rest) -> proc (f acc i info.nodeData) rest
      (Node i _ info children :: rest) -> proc (f acc i info.nodeData) (append rest children)
  in proc acc [node]


foldPostOrder : (acc -> Int -> d -> acc) -> acc -> Node d -> acc
foldPostOrder f acc node =
  let proc acc node =
    case node of
      (Leaf i _ info) -> f acc i info.nodeData
      (Node i _ info children) ->
        let acc1 = foldl (\n lacc -> proc lacc n) acc children
        in f acc1 i info.nodeData
  in proc acc node


foldPreOrder : (acc -> Int -> d -> acc) -> acc -> Node d -> acc
foldPreOrder f acc node =
  let proc acc node =
    case node of
      (Leaf i _ info) -> f acc i info.nodeData
      (Node i _ info children) ->
        let acc1 = f acc i info.nodeData
        in foldl (\n lacc -> proc lacc n) acc1 children
  in proc acc node

--foldr : (a -> b -> b) -> b -> List a -> b

{-}

In-order[edit]

In-order: A, B, C, D, E, F, G, H, I.
Check if the current node is empty / null.
Traverse the left subtree by recursively calling the in-order function.
Display the data part of the root (or current node).
Traverse the right subtree by recursively calling the in-order function.


export function Node(data) {
  this.data = data;
  this.depth =
  this.height = 0;
  this.parent = null;
}
-}