module D3Elm.Shapes.Curves.Natural exposing (..)

import Basics exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Shapes.Curves.Linear exposing (..)





initialAs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [2]
          (ki::ki1::kr) -> 1::go (ki1::kr)
  in case ks of
      (k0::kr) -> 0::go kr

initialBs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [7]
          (ki::ki1::kr) -> 4::go (ki1::kr)
  in case ks of
      (k0::kr) -> 2::go kr


initialCs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [0]
          (ki::ki1::kr) -> 1::go (ki1::kr)
  in case ks of
      (k0::kr) -> 1::go kr
      _ -> []

initialDs ks =
  let go ks =
        case ks of
          (kn1::kn::[]) -> [8 * kn1 + 2 * kn]
          (ki::ki1::kr) -> (4 * ki + 2 * ki1)::go (ki1::kr)
  in case ks of
      (k0::k1::kr) -> (k0 + 2 * k1)::go (k1::kr)


sweepCs a b c =
  let go nc previousC a b c =
        case (a,b,c) of
          ([],[],[]) -> nc
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
          ([],[],[], []) -> nc
          (ai::ar, bi::br, ci::cr, di::dr) ->
            let ndi = (di - ai * previousD) / (bi - ai * previousC)
            in go (ndi::nd) nci ci ar br cr dr
          _ -> []
  in case (a,b,c,d) of
      (ai::ar, bi::br, ci::cr, di::dr) ->
        let ndi = ci / bi
        in go [ndi] ndi ci ar br cr dr
      _ -> []



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

