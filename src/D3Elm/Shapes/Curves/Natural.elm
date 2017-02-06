module D3Elm.Shapes.Curves.Natural exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Curves.Linear exposing (..)



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

{-


function controlPoints(x) {
  var i,
      n = x.length - 1,
      m,
      a = new Array(n),
      b = new Array(n),
      r = new Array(n);

  a[0] = 0, b[0] = 2, r[0] = x[0] + 2 * x[1];

  for (i = 1; i < n - 1; ++i)
    a[i] = 1,
    b[i] = 4,
    r[i] = 4 * x[i] + 2 * x[i + 1];

  a[n - 1] = 2,
  b[n - 1] = 7,
  r[n - 1] = 8 * x[n - 1] + x[n];

  for (i = 1; i < n; ++i)
    m = a[i] / b[i - 1],
    b[i] -= m,
    r[i] -= m * r[i - 1];

  a[n - 1] = r[n - 1] / b[n - 1];


  for (i = n - 2; i >= 0; --i)
    a[i] = (r[i] - a[i + 1]) / b[i];

  b[n - 1] = (x[n] + a[n - 1]) / 2;

  for (i = 0; i < n - 1; ++i)
    b[i] = 2 * x[i + 1] - a[i + 1];

  return [a, b];
}
-}

