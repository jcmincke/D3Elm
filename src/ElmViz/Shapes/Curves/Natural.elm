module ElmViz.Shapes.Curves.Natural exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import ElmViz.Path.Path exposing (..)

import List as L exposing (..)
import Tuple  exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Path.Path exposing (..)

natural ks path =
  case ks of
    [] -> path
    [(x0, y0)] -> moveTo (x0, y0) path
    [(x0, y0), (x1, y1)] -> lineTo (x1, y1) <| moveTo (x0, y0) path
    _ ->
      let xks = L.map first ks
          yks = L.map second ks
          xs = controlPoints xks
          ys = controlPoints yks

          go path xs ys =
            case (xs, ys) of
              ([], []) -> path
              ((x0,x1,x2,x3)::xsr, (y0,y1,y2,y3)::ysr) ->
                  let path1 = cubicCurveTo (x1, y1) (x2, y2) (x3, y3) path
                  in go path1 xsr ysr
              _ -> path

      in case (xs, ys) of
          ((x0,_,_,_)::_, (y0,_,_,_)::_) ->
            let path1 = moveTo (x0, y0) path
            in go path1 xs ys
          _ -> path

controlPoints ks =
  let a = initialAs ks
      b = initialBs ks
      c = initialCs ks
      d = initialDs ks

      c1 = sweepCs a b c
      d1 = sweepDs a b c1 d
      p1 = backSubstitution c1 d1

      p2 = computeP2 ks p1
      go ks p1 p2 =
        case (ks, p1, p2) of
          (ki::ki1::[], [p1i], [p2i]) -> [(ki, p1i, p2i, ki1)]
          (ki::ki1::kr, p1i::p1r, p2i::p2r) ->
            (ki, p1i, p2i, ki1) :: go (ki1::kr) p1r p2r
          _ -> []
  in go ks p1 p2

initialAs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [2]
          (ki::ki1::kr) -> 1::go (ki1::kr)
          _ -> []
  in case ks of
      (k0::kr) -> 0::go kr
      _ -> []

initialBs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [7]
          (ki::ki1::kr) -> 4::go (ki1::kr)
          _ -> []
  in case ks of
      (k0::kr) -> 2::go kr
      _ -> []


initialCs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [0]
          (ki::ki1::kr) -> 1::go (ki1::kr)
          _ -> []
  in case ks of
      (k0::kr) -> 1::go kr
      _ -> []

initialDs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [8 * kn1 + kn]
          (ki::ki1::kr) -> (4 * ki + 2 * ki1)::go (ki1::kr)
          _ -> []
  in case ks of
      (k0::k1::kr) -> (k0 + 2 * k1)::go (k1::kr)
      _ -> []


sweepCs a b c =
  let go nc previousC a b c =
        case (a,b,c) of
          ([],[],[]) -> L.reverse nc
          (ai::ar, bi::br, ci::cr) ->
            let nci = ci / (bi - ai * previousC)
            in go (nci::nc) nci ar br cr
          _ -> []
  in case (a,b,c) of
      (ai::ar, bi::br, ci::cr) ->
        let nci = ci / bi
        in go [nci] nci ar br cr
      _ -> []


sweepDs a b c d =
  let go nd previousD previousC a b c d =
        case (a,b,c,d) of
          ([],[],[], []) -> L.reverse nd
          (ai::ar, bi::br, ci::cr, di::dr) ->
            let ndi = (di - ai * previousD) / (bi - ai * previousC)
            in go (ndi::nd) ndi ci ar br cr dr
          _ -> []
  in case (a,b,c,d) of
      (ai::ar, bi::br, ci::cr, di::dr) ->
        let ndi = di / bi
        in go [ndi] ndi ci ar br cr dr
      _ -> []

backSubstitution c d =
  let go c d =
        case (c, d) of
          ([_], [dn]) -> (dn, [dn])
          (ci::cr, di::dr) ->
            let (nextX, xs) = go  cr dr
                nx = di - ci * nextX
            in (nx, nx::xs)
          _ -> (0, [])
      (_, xs) = go c d
  in xs


computeP2 ks p =
  let go ks p =
        case (ks, p) of
          (kn1::kn::[], [pn1]) -> [(2 * kn1 - pn1), (kn+pn1)/2]
          (ki::ki1::kr, pi::pr) -> 2 * ki - pi :: go (ki1::kr) pr
          _ -> [999]
  in case (ks, p) of
      (k0::kr, p0::pr) -> go kr pr -- (2 * p0 - k0) :: go ks p
      _ -> [998]

computeKs p1 p2 =
  let go p1 p2 =
    case (p1, p2) of
      ([p1n1], [p2n1]) -> [2 * p2n1 - p1n1]
      (_::p1i::p1r, p2i1::p2i::p2r) -> (p1i + p2i1)/2 :: go (p1i::p1r) (p2i::p2r)
      _ -> [997]
  in case (p1, p2) of
      (p10::p1r, p20::p2r) -> 2 * p10 - p20 :: go p1 p2
      _ -> [996]




checkD a b c x =
  let x1 = 0 :: L.reverse (0::L.reverse x) -- add 0 at both end of x
      go a b c x =
        case (a,b,c,x) of
          (ai::ar, bi::br, ci::cr, xi::xi1::xi2::xr) ->
              (ai * xi + bi * xi1 + ci * xi2 :: go ar br cr (xi1::xi2::xr))
          _ -> []
      d1 = go a b c x1
  in d1

