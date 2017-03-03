module D3Elm.Geo.Common exposing (..)

import GeoJson exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)



closingMap : (a -> b) -> List a -> List b
closingMap f l =
  let go b0 =
        let go1 l =
          case l of
            [] -> [b0]
            (h::r) -> f h :: go1 r
          in go1
  in case l of
    [] -> []
    [a] -> [f a]
    (h::r) -> let b0 = f h in b0 :: go b0 r

-- todo move to D3.Common

mkList s e last n =
  let d = (e - s) / n
      go x0 n =
        if n == 0
        then last
        else x0 :: go (x0 + d) (n-1)
  in go s n

toGeoPosition : (Float, Float) -> Position
toGeoPosition (a, b) = (a, b, 0)
















