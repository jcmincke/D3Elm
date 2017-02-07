module D3Elm.Voronoi.Voronoi exposing (..)

import Lazy as L
import List exposing (append, foldl, reverse, maximum, map, head, sortWith)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)


type alias Point = {
    x : Float
    , y : Float
  }

type Site = Site Int Point


type Tree =
  Break Tree Tree Site Site
  | Leaf Site


type ArcPred =
  Ok
  |OnLeft
  |Onright

-- insert an arc

insertArc : Site -> (Site -> ArcPred) -> Tree -> Tree
insertArc c arcSelector tree =
  let go tree =
    case tree of
      Leaf b ->
        if arcSelector b == Ok  -- ok replace this arc
        then (Break (Leaf b) (Break (Leaf c) (Leaf b) c b) b c, True)
        else (Leaf b, False)
      Break ta tb a b ->
        let (nta, found) = go ta
        in  if found
            then (Break nta tb a b, True)
            else  let (ntb, found) = go tb
                  in  if found
                      then (Break ta ntb a b, True)
                      else (tree, False)
  in case tree of
    Leaf a -> Break (Leaf a) (Break (Leaf c) (Leaf a) c a) a c
    Break _ _ _ _ -> first (go tree)


-- parabola equation
parabola yd (xf, yf) x =
  let p = yf - yd
  in (x-xf)^2 / (2 * p) + yd + (yf + yd) / 2

-- finding intersection between 2 parabolas

parabolaIntersection yd (xp1, yp1) (xp2, yp2) =
  let p1 = yp1 - yd
      p2 = yp2 - yd
      a = p2 - p1
      b = 2 * (p1 * xp2 - p2 * xp1)
      c = p2 * xp1 * xp1 - p1 * xp2 * xp2 + p1 * p2 * (yp1 - yp2)
      rm = solve2 a b c
  in rm




-- solving a 2nd degree equation
-- a x^2 + b x + c = 0

solve2 a b c =
  let d2 = b * b - 4 * a * c
  in if d2 < 0
     then Nothing
     else let d = sqrt d2
              x1 = ((-1) * b - d) / (2 * a)
              x2 = ((-1) * b + d) / (2 * a)
          in Just (x1, x2)


