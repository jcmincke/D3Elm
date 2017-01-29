module D3Elm.Hierarchy.Tree.Tree exposing (..)

import Lazy as L
import List exposing (append, foldl, reverse, maximum, map, head)
import Dict as D exposing (insert, empty, Dict, get)


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

findNodes : (Node d -> Bool) -> Node d -> List (Node d)
findNodes pred node =
  let proc acc n =
    if pred n
    then n::acc
    else acc
  in foldPreOrder proc [] node

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

getParents : Node d -> Dict Int (Node d)
getParents n =
  let proc acc n =
    case n of
      (Leaf _ _) -> acc
      (Node i _ cs) -> foldl (\c acc1 -> insert (getIndex c) n acc1) acc cs
  in foldPostOrder proc empty n


computeHeight : Node d -> Node d
computeHeight node =
  let proc n =
        case n of
          (Leaf i info) -> Leaf i {info | nodeHeight = 0}
          (Node i info cs) ->
            let cs1 = map proc cs
                h = maxHeight cs1
            in Node i {info | nodeHeight = h+1} cs1
      maxHeight nodes =
         case maximum <| map getHeight nodes of
          Just h -> h
          Nothing -> 0
  in proc node

getLeaves : Node d -> List (Node d)
getLeaves node =
  let proc acc n =
    case n of
      (Leaf _ _ as l) -> l::acc
      (Node _ _ _) -> acc
  in foldPostOrder proc [] node


getAncestors : Dict Int (Node d) -> Node d -> List (Node d)
getAncestors parents node =
  let proc acc n =
        let i = getIndex n
        in case D.get i parents of
            Just p -> proc (p::acc) p
            Nothing -> acc
  in proc [] node

getDescendants : Node d -> List (Node d)
getDescendants node =
  let proc acc n = n::acc
  in foldBreadthFirst proc [] node

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