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




crossProduct2D : (Float, Float) -> (Float, Float) -> Float
crossProduct2D (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

crossProduct3D : (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
crossProduct3D (x1, y1, z1) (x2, y2, z2) =
    (y1*z2 - y2*z1, x2*z1 - x1*z2, x1*y2 - x2*y1)



-- Solve a 2nd degree equation
-- a x^2 + b x + c = 0
solve2 : Float -> Float -> Float -> Maybe (Float, Float)
solve2 a b c =
  let d2 = b * b - 4 * a * c
  in if d2 < 0
     then Nothing
     else let d = sqrt d2
              x1 = ((-1) * b - d) / (2 * a)
              x2 = ((-1) * b + d) / (2 * a)
          in Just (x1, x2)

