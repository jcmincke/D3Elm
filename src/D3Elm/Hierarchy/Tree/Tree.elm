module D3Elm.Hierarchy.Tree.Tree exposing (..)

import Lazy as L
import List exposing (append, foldl, reverse)
import Dict exposing (insert, empty, Dict)


type alias NodeInfo data = {
  nodeData : data
  , nodeDepth : Int
  , nodeHeight : Int
}

type Node d =
  Node Int (L.Lazy (Maybe Node)) (NodeInfo d) (List (Node d))
  |Leaf Int (L.Lazy (Maybe Node)) (NodeInfo d)

getInfo : Node d -> NodeInfo d
getInfo n =
  case n of
    (Leaf _ _ info) -> info
    (Node _ _ info _) -> info

getIndex : Node d -> Int
getIndex n =
  case n of
    (Leaf i _ _) -> i
    (Node i _ _ _) -> i

getData : Node d -> d
getData n = (getInfo n).nodeData

-- construct a Node

buildTree : a -> (a -> d) -> (a -> List a) -> Node d
buildTree a fvalue fchildren =
  let noParent = (L.lazy (\() -> Nothing))
      proc index depth a =
        let n =
          case fchildren a of
            [] -> Leaf index noParent (NodeInfo (fvalue a) depth 0)
            children ->
              let (ns, index1) = foldl (\a (acc, i) -> (proc i (depth+1) a::acc, i+1)) ([], index+1) children
              in Node index1 noParent (NodeInfo (fvalue a) depth 0) (reverse ns)
        in n
  in proc 0 0 a

parents : Node d -> Dict Int (Node d)
parents n =
  let proc acc n =
    case n of
      (Leaf _ _ _) -> acc
      (Node i _ _ cs) -> foldl (\c acc -> insert (getIndex c) n acc) acc cs
  in foldBreadthFirst proc empty n



--
--setParents : Node d -> Node d
--setParents n =
--  let proc p n =
--    case n of
--      (Leaf i _ info) -> Leaf i p info
--      (Node i _ info cs) -> let n1 = Node i p info


foldBreadthFirst : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldBreadthFirst f acc node =
  let proc acc nodes =
    case nodes of
      [] -> acc
      (Leaf _ _ _ as n) :: rest -> proc (f acc n) rest
      (Node _ _ _ children as n) :: rest -> proc (f acc n) (append rest children)
  in proc acc [node]


foldPostOrder : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldPostOrder f acc node =
  let proc acc node =
    case node of
      (Leaf _ _ _ as n) -> f acc n
      (Node _ _ _ children as n) ->
        let acc1 = foldl (\n lacc -> proc lacc n) acc children
        in f acc1 n
  in proc acc node


foldPreOrder : (acc -> Node d -> acc) -> acc -> Node d -> acc
foldPreOrder f acc node =
  let proc acc node =
    case node of
      (Leaf _ _ _ as n) -> f acc n
      (Node _ _ _ children as n) ->
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