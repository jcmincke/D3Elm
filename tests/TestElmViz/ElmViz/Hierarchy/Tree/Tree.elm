module TestElmViz.ElmViz.Hierarchy.Tree.Tree exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import List as L exposing (reverse, all, map, concat)
import Dict as D exposing (isEmpty, Dict, get)


import ElmViz.Hierarchy.Tree.Tree exposing (..)


{-
type alias NodeInfo data = {
  nodeData : data
  , nodeDepth : Int
  , nodeHeight : Int
}

type Node d =
  Node Int (L.Lazy (Maybe Node)) (NodeInfo d) (List (Node d))
  |Leaf Int (L.Lazy (Maybe Node)) (NodeInfo d)

-}

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


testPath () =
  let
    parents = getParents node
    n1s = findNodes (\n -> getIndex n == 7) node
    n2s = findNodes (\n -> getIndex n == 3) node
  in case (n1s, n2s) of
        ([n1], [n2]) ->
          let (p11, p21) = getPath node n1 n2
              (p22, p12) = getPath node n2 n1
              d11 = map getData p11
              d21 = map getData p21
              d12 = map getData p12
              d22 = map getData p22
              anc1 = map getData <| getAncestors parents n1
              anc2 = map getData <| getAncestors parents n2
              b1 = (d11, d21) == (["F","G","I"],["F","B","D"])
              b2 = (d11, d21) == (d12, d22)
          in Expect.true "" (b1 && b2)
        _ -> Expect.fail "bug"


all : Test
all =
    describe "Tree "
        [ test "foldBreadthFirst" <| testFoldBreadthFirst
          , test "foldPostOrder" <| testFoldPostOrder
          , test "foldPreOrder" <| testFoldPreOrder
          , test "buildTree" <| testBuildTree
          , test "computeHeight" <| testComputeHeight
          , test "getLeaves" <| testGetLeaves
          , test "findNodes" <| testFindNodes
          , test "getAncestors" <| testGetAncestors
          , test "testGetDescendants" <| testGetDescendants
          , test "sum" <| testSum
          , test "count" <| testCount
          , test "sort" <| testSort
          , test "links" <| testLinks
          , test "path" <| testPath



        ]



testSort () = let
    n1 = sort (\n1 n2 -> compare (getIndex n2) (getIndex n1)) node
    r = reverse <| foldPreOrder (\acc n -> getData n::acc) [] n1
  in Expect.equal r ["F", "G", "I", "H", "B", "A", "D", "E", "C" ]


testLinks () = let
    r = reverse <| map (\(a,b) -> (getData a, getData b)) <| getLinks node
  in Expect.equal r [
      ("F", "B")
      , ("F", "G")
      , ("B", "A")
      , ("B", "D")
      , ("G", "I")
      , ("D", "C")
      , ("D", "E")
      , ("I", "H")
      ]

testFindNodes () = let
    r = map getData <| findNodes (\n -> getIndex n == 4) node
  in Expect.equal r ["A"]

testFoldBreadthFirst () = let
    r = reverse <| foldBreadthFirst (\acc n -> getData n::acc) [] node
  in Expect.equal r ["F", "B", "G", "A", "D", "I", "C", "E", "H"]



testFoldPostOrder () = let
    r = reverse <| foldPostOrder (\acc n -> getData n::acc) [] node
  in Expect.equal r ["A", "C", "E", "D", "B", "H", "I", "G", "F"]


testFoldPreOrder () = let
    r = reverse <| foldPreOrder (\acc n -> getData n::acc) [] node
  in Expect.equal r ["F", "B", "A", "D", "C", "E", "G", "I", "H"]


testGetLeaves () = let
    r = reverse <| map getData <| getLeaves node
  in Expect.equal r ["A", "C", "E", "H"]



testGetAncestors () =
    case findNodes (\n -> getIndex n == 6) node of
      [nh] ->
        let parents = getParents node
            r = reverse <| map getData <| getAncestors parents nh
        in Expect.equal r ["H", "I", "G", "F"]
      _ -> Expect.equal  1 2

testGetDescendants () =
  let r = reverse <| map getData <| getDescendants node
  in Expect.equal r ["F", "B", "G", "A", "D", "I", "C", "E", "H"]

testSum () =
  let r = sum getIndex (+) 0 node
  in Expect.equal (D.get 9 r) (Just 45)


testCount () =
  let r = count node
  in Expect.equal (D.get 9 r) (Just 4)

testBuildTree () =
  let fchildren n =
        case n of
        (Node _ _ cs) -> cs
        _ -> []
      fvalue n =
        case n of
        (Node _ info _) -> info.nodeData
        (Leaf _ info) -> info.nodeData
      t1 = buildTree node fvalue fchildren
      t2 = buildTree t1 fvalue fchildren
      t3 = buildTree t2 fvalue fchildren
  in Expect.true "" <| nodeStructEq t2 t3


testComputeHeight () =
  let t1 = computeHeight node
      t2 = computeHeight t1
      t3 = computeHeight t2
  in Expect.equal node t3

nodeStructEq : Node d -> Node d -> Bool
nodeStructEq a b =
  let eqList la lb =
        case (la, lb) of
          ([], []) -> True
          (a::ra, b::rb) -> nodeStructEq a b && eqList ra rb
          _ -> False
  in case (a,b) of
      (Leaf i1 info1, Leaf i2 info2) -> i1 == i2 && info1 == info2
      (Node i1 info1 cs1, Node i2 info2 cs2) ->
        let b1 = i1 == i2 && info1 == info2
        in  if b1
            then eqList cs1 cs2
            else False
      _ -> False

