module D3Elm.Voronoi.Voronoi exposing (..)

import List as L exposing (append, foldl, reverse, maximum, map, head, sortWith, reverse)
import Dict as D exposing (insert, empty, Dict, get)
import Maybe exposing (withDefault)
import Tuple exposing (..)


type alias Point = {
    x : Float
    , y : Float
  }

type Site = Site Int Point


type Tree site =
  Break (Tree site) (Tree site) site site
  | Leaf site


type ArcPred =
  ArcFound
  |OnLeft
  |Onright

-- returned the sites in the left/right order

allSites : Tree site -> List site
allSites tree =
  let go acc tree =
    case tree of
      Leaf s -> s::acc
      Break ta tb _ _ -> go (go acc ta) tb
  in L.reverse <| go [] tree

type OpenOn = OpenOnRight | OpenOnLeft

-- find the right most break having a leaf as its right children
-- returns the break's sites
rightMostBreak : site -> site -> (site -> site -> site -> ArcPred)
  -> (OpenOn -> site -> site -> ArcPred)
  -> Tree site -> (Tree site, Bool)
rightMostBreak ns b intersectPredClosed intersectPredOpen tree =
  let go tree =
        case tree of
          Leaf a -> -- break is the left most
            case intersectPredOpen OpenOnLeft a b of
              ArcFound -> -- ok, we can split this arc.
                  (Break (Leaf a) (Break (Leaf ns) (Leaf a) ns a) a ns, True)
              _ -> (tree, False)
          Break t1 (Leaf _) c a ->
              -- we have 2 consecutive breaks and 3 sites (c, a, b) so we have a piece of a parabola
              -- defined for the site a
              -- find whether the new site intersects that parabol
              case intersectPredClosed c a b of
                ArcFound -> -- ok, we can split this arc.
                  (Break t1
                        (Break (Leaf a) (Break (Leaf ns) (Leaf a) ns a) a ns)
                        c a
                    , True)
                _ -> (tree, False) -- continue to look for arc
          Break t1 t2 s1 s2 ->
            let (ts2, bool) = go t2
            in (Break t1 ts2 s1 s2, bool)
  in go tree


-- find the left most break having a leaf as its left children
-- returns the break's sites
leftMostBreak :
  site -> site
  -> (site -> site -> site -> ArcPred)
  -> (OpenOn -> site -> site -> ArcPred)
  -> Tree site
  -> (Tree site, Bool)
leftMostBreak ns b intersectPredClosed intersectPredOpen tree =
  let go tree =
        case tree of
          Leaf c -> -- break is the left most
            case intersectPredOpen OpenOnRight b c of
              ArcFound -> -- ok, we can split this arc.
                  (Break (Leaf b) (Break (Leaf ns) (Leaf b) ns b) b ns, True)
              _ -> (tree, False)
          Break (Leaf _) t2 a c ->
              -- we have 2 consecutive breaks and 3 sites (b, a, c) so we have a piece of a parabola
              -- defined for the site a
              -- find whether the new site intersects that parabol
              case intersectPredClosed b a c of
                ArcFound -> -- ok, we can split this arc.
                  (Break
                      (Break (Leaf a) (Break (Leaf ns) (Leaf a) a ns) ns a)
                      t2
                      a c
                    , True)
                _ -> (tree, False) -- continue to look for arc
          Break t1 t2 s1 s2 ->
            let (ts1, bool) = go t1
            in (Break ts1 t2 s1 s2, bool)
  in go tree



-- insert an arc


insertArc : site -> (site -> site -> site -> ArcPred) -> (OpenOn -> site -> site -> ArcPred)
  -> Tree site -> Tree site
insertArc ns intersectPredClosed intersectPredOpen tree =
  let go tree =
      case tree of
        Leaf b -> (tree, False)
        Break ta tb a b ->
          -- find the arc between this break and the previous break.
          let (ta1, bool) = rightMostBreak ns b intersectPredClosed intersectPredOpen ta
          in  if bool
              then (Break ta1 tb a b, True)
              else -- try on the right
                let (tb1, bool) = leftMostBreak ns a intersectPredClosed intersectPredOpen tb
                in  if bool
                    then (Break ta tb1 a b, True)
                    else -- intersection not found in the neighborood, continue from break on the and on the right
                      case go ta of
                        (ta11, True) -> (Break ta1 tb a b, True)
                        _ -> case go tb of
                              (tb11, True) -> (Break ta tb1 a b, True)
                              _ -> (tree, False)
  in case tree of
    Leaf a -> Break (Leaf a) (Break (Leaf ns) (Leaf a) ns a) a ns
    Break _ _ _ _ ->
      let (t, _) = go tree
      in t


{-

insertArc : site -> (acc -> Tree site -> acc) -> acc -> (acc -> site -> ArcPred) -> Tree site -> Tree site
insertArc c visit acc arcSelector tree =
  let go n tree =
    let acc1 = visit acc tree
    in case tree of
        Leaf b ->
          if arcSelector acc1 b == Ok  -- ok replace this arc
          then (Break (Leaf b) (Break (Leaf c) (Leaf b) c b) b c, acc1, True)
          else (Leaf b, acc1, False)
        Break ta tb a b ->
          let (nta, acc2, found) = go acc1 ta
          in  if found
              then (Break nta tb a b, acc2, True)
              else  let (ntb, acc3, found) = go acc2 tb
                    in  if found
                        then (Break ta ntb a b, acc3, True)
                        else (tree, acc3, False)
  in case tree of
    Leaf a -> Break (Leaf a) (Break (Leaf c) (Leaf a) c a) a c
    Break _ _ _ _ ->
      let (t, _, _) = go acc tree
      in t

-}
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


