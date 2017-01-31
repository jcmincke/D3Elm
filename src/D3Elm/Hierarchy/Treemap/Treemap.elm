module D3Elm.Hierarchy.Treemap.Treemap exposing (..)

import List as L
import List exposing (append, foldl, reverse, maximum, map, head, sortWith)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)

import D3Elm.Hierarchy.Tree.Tree exposing (..)

splitBox : Float -> Float -> List (Int, Float) -> List (Int, Float, Float)
splitBox x1 x2 iweights =
  case iweights of
    [] -> []
    ((i, wh)::wr) ->
      let total = foldl (+) 0 <| L.map (\(_, w) -> w) iweights
          ratio = (x2-x1)/total
          (_, _, xs) = foldl (\(i, w) (cw, xp, xs) ->
              let nw = cw+w
                  xn = x1 + nw * ratio
              in (nw, xn, (i, xp, xn)::xs)) (wh,  x1 + wh * ratio, [(i, x1, x1 + wh * ratio)]) wr
      in reverse xs


getVal : a -> Dict Int a -> Node d -> a
getVal def dic node = withDefault def <| D.get (getIndex node) dic

sliceAndDice : Float -> (Node d -> Float) -> Node d -> Dict Int ((Float, Float), (Float, Float))
sliceAndDice ratio cost node =
  let defBox = ((0,0), (1,1))
      valueDic = sum cost (+) 0 node
      procV ((x1, y1), (x2, y2)) boxDic n =
        case n of
          Leaf i _ -> D.insert i ((x1, y1), (x2, y2)) boxDic
          Node i _ cs ->
            let iweights = L.map (\n -> (getIndex n, getVal 0 valueDic n)) cs
                ixs = splitBox x1 x2 iweights
                boxDic1 = L.foldl (\(i, xa, xb) dic -> D.insert i ((xa, y1), (xb, y2)) dic) boxDic ixs
                foldProc n boxDic =
                  let box = getVal defBox boxDic n
                  in procH box boxDic n
            in L.foldl foldProc boxDic1 cs
      procH ((x1, y1), (x2, y2)) boxDic n =
        case n of
        Leaf i _ -> D.insert i ((x1, y1), (x2, y2)) boxDic
        Node i _ cs ->
          let iweights = L.map (\n -> (getIndex n, getVal 0 valueDic n)) cs
              ixs = splitBox y1 y2 iweights
              boxDic1 = L.foldl (\(i, ya, yb) dic -> D.insert i ((x1, ya), (x2, yb)) dic) boxDic ixs
              foldProc n boxDic =
                let box = getVal defBox boxDic n
                in procV box boxDic n
          in L.foldl foldProc boxDic1 cs
  in procV defBox empty node


