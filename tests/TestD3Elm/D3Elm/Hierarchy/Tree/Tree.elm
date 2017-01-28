module TestD3Elm.D3Elm.Hierarchy.Tree.Tree exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Lazy as L
import List exposing (reverse, all, map, concat)


import D3Elm.Hierarchy.Tree.Tree exposing (..)


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



all : Test
all =
    describe "Tree "
        [ test "foldBreadthFirst" <| testFoldBreadthFirst
          , test "foldPostOrder" <| testFoldPostOrder
          , test "foldPreOrder" <| testFoldPreOrder
          , test "buildTree" <| testBuildTree
          , test "computeHeight" <| testComputeHeight
        ]


testFoldBreadthFirst () = let
    r = reverse <| foldBreadthFirst (\acc n -> getData n::acc) [] node
  in Expect.equal r ["F", "B", "G", "A", "D", "I", "C", "E", "H"]


testFoldPostOrder () = let
    r = reverse <| foldPostOrder (\acc n -> getData n::acc) [] node
  in Expect.equal r ["A", "C", "E", "D", "B", "H", "I", "G", "F"]

testFoldPreOrder () = let
    r = reverse <| foldPreOrder (\acc n -> getData n::acc) [] node
  in Expect.equal r ["F", "B", "A", "D", "C", "E", "G", "I", "H"]


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
  let t1s = computeHeight node
      t2s = List.concat <| map computeHeight t1s
      t3s = List.concat <| map computeHeight t2s
--      r = map
  in Expect.equal t2s t3s
  --Expect.true "" True -- <| nodeStructEq t2 t3

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


{-


type Node d =
  Node Int (L.Lazy (Maybe Node)) (NodeInfo d) (List (Node d))
  |Leaf Int (L.Lazy (Maybe Node)) (NodeInfo d)



buildTree : a -> (a -> d) -> (a -> List a) -> Node d


        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]

-}