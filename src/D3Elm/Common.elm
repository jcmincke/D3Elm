module D3Elm.Common exposing (..)

import Basics exposing (..)
import List as L exposing (..)

zip: List a -> List b -> List (a,b)
zip la lb = zipWith (,) la lb


zipWith: (a -> b -> c) -> List a -> List b -> List c
zipWith f la lb =
  case (la, lb) of
    ([], _) -> []
    (_, []) -> []
    (ha::ta, hb::tb) -> (f ha hb) :: zipWith f ta tb

mkList : Int -> Int -> List Int
mkList s e =
  let go i =
    if i == e then [e] else i::go (i+1)
  in go s




