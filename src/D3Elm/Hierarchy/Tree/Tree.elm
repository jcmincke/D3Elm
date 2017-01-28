module D3Elm.Hierarchy.Tree.Tree exposing (..)

import Lazy as L
import List exposing (append, foldl, reverse, maximum, map, head)
import Dict exposing (insert, empty, Dict)


type alias NodeInfo data = {
  nodeData : data
  , nodeDepth : Int
  , nodeHeight : Int
}

type Node d =
  Node Int (NodeInfo d) (List (Node d))
  |Leaf Int (NodeInfo d)

getInfo : Node d -> NodeInfo d
getInfo n =
  case n of
    (Leaf _ info) -> info
    (Node _ info _) -> info

getIndex : Node d -> Int
getIndex n =
  case n of
    (Leaf i _) -> i
    (Node i _ _) -> i

getData : Node d -> d
getData n = (getInfo n).nodeData

getDepth : Node d -> Int
getDepth n = (getInfo n).nodeDepth

getHeight : Node d -> Int
getHeight n = (getInfo n).nodeHeight

-- construct a Node

buildTree : a -> (a -> d) -> (a -> List a) -> Node d
buildTree a fvalue fchildren =
  let proc index depth a =
        let n =
          case fchildren a of
            [] -> Leaf index (NodeInfo (fvalue a) depth 0)
            children ->
              let (ns, index1) = foldl (\a (acc, i) -> (proc i (depth+1) a::acc, i+1)) ([], index+1) children
              in Node index1 (NodeInfo (fvalue a) depth 0) (reverse ns)
        in n
  in proc 0 0 a

parents : Node d -> Dict Int (Node d)
parents n =
  let proc acc n =
    case n of
      (Leaf _ _) -> acc
      (Node i _ cs) -> foldl (\c acc -> insert (getIndex c) n acc) acc cs
  in foldBreadthFirst proc empty n


computeHeight : Node d -> List (Node d)
computeHeight node =
  let proc acc n =
        case n of
          (Leaf i info) -> Leaf i {info | nodeHeight = 0}::acc
          (Node i info cs) ->
            let h = maxHeight acc
            in [Node i {info | nodeHeight = h+1} (reverse acc)]
      maxHeight nodes =
         case maximum <| map getHeight nodes of
          Just h -> h
          Nothing -> 0
      r = foldPostOrder proc [] node
  in r




foldBreadthFirst : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldBreadthFirst f acc node =
  let proc acc nodes =
    case nodes of
      [] -> acc
      (Leaf _ _ as n) :: rest -> proc (f acc n) rest
      (Node _ _ children as n) :: rest -> proc (f acc n) (append rest children)
  in proc acc [node]


foldPostOrder : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldPostOrder f acc node =
  let proc acc node =
    case node of
      (Leaf _ _ as n) -> f acc n
      (Node _ _ children as n) ->
        let acc1 = foldl (\n lacc -> proc lacc n) acc children
        in f acc1 n
  in proc acc node


foldPreOrder : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldPreOrder f acc node =
  let proc acc node =
    case node of
      (Leaf _ _ as n) -> f acc n
      (Node _ _ children as n) ->
        let acc1 = f acc n
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