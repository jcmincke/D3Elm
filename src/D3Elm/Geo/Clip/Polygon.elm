
module D3Elm.Geo.Clip.Polygon exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)


-- Sutherland–Hodgman algorithm for polygon clipping

clipPolygon : [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
clipPolygon


goOneClippingEdge : (Float, Float) -> (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
goOneClippingEdge ((xs, ys) as ps) ((xe, ye) as pe) pts =
  let go acc pts =
    case pts of
    (((xps, yps) as pps)::((xpe, ype) as ppe::r) ->
      if isInsideClipEdge ps pe ppe
      then  if not (isInsideClipEdge ps pe pps)
            then





isInsideClipEdge (xs, ys) (xe, ye) (x, y) =
  vectProduct (xe-xs, ye-ys) (x-xs, y-ys) > 0



List outputList = subjectPolygon;
  for (Edge clipEdge in clipPolygon) do
     List inputList = outputList;
     outputList.clear();
     Point S = inputList.last;
     for (Point E in inputList) do
        if (E inside clipEdge) then
           if (S not inside clipEdge) then
              outputList.add(ComputeIntersection(S,E,clipEdge));
           end if
           outputList.add(E);
        else if (S inside clipEdge) then
           outputList.add(ComputeIntersection(S,E,clipEdge));
        end if
        S = E;
     done
  done

y = a x + c
y = b x + d




xi = (d-c)/(a-b)
yi = (ad-bc)/(a-b)


x0, y0

lineIntersection (x1s, y1s) (x1e, y1e) (x2s, y2s) (x2e, y2e) =
  let a = (y1e - y1s) / (x1e - x1s)



  x0 -> x1

  x = x0 + t (x1 - x0) -> t = (x-x0) / (x1-x0)
  x = y0 + t (y1 - y0) -> t = (y-y0) / (y1-y0)


(x-x0) / (x1-x0) = (y-y0) / (y1-y0)

(x-x0) (y1-y0) = (y-y0) (x1-x0)

x (y1-y0) - x0 (y1 -y0) = y (x1-x0) - y0 (x1-x0)

(y1-y0) x - (x1-x0) y + y0 (x1-x0) - x0 (y1-y0) = 0


y = (y1-y0) / (x1-x0) * x + y0 - x0 * (y1-y0) / (x1-x0)

a = ()

p0 = (0, -2)
p1 = (2, 0)

C = -2 (0 - 2) = 4 +


p0 = (0, 1)
p1 = (1, 0)

c = 1 (1-0) = 1





