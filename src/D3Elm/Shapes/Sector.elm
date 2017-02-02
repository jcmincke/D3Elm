module D3Elm.Shapes.Sector exposing (..)

import Basics exposing (..)

import D3Elm.Path.Path exposing (..)


type alias SectorSpec = {
    innerRadius : Float
    , outerRadius : Float
    , cornerRadius : Float
    , startAngle : Float
    , endAngle : Float
    , padAngle : Float
  }

sectorSpec0 : SectorSpec
sectorSpec0 = {
    innerRadius = 0
    , outerRadius = 1
    , cornerRadius = 0
    , startAngle = 0
    , endAngle = pi/6
    , padAngle = 0
  }


sector spec =
  if spec.endAngle - spec.startAngle >= 2 * pi
  then
    -- full circle
    let opp (x,y) = (-x, -y)
        ((x1, y1) as p1) = (spec.innerRadius * cos spec.startAngle, spec.innerRadius * sin spec.startAngle)
        ((x2, y2) as p2) = (spec.outerRadius * cos spec.startAngle, spec.outerRadius * sin spec.startAngle)
        path1 = moveTo p1 path0
        path2 = arcCurveTo spec.innerRadius spec.innerRadius 0 True True (opp p1) path1
        path3 = arcCurveTo spec.innerRadius spec.innerRadius 0 True True p1 path2
        path4 = moveTo p2 path3
        path5 = arcCurveTo spec.outerRadius spec.outerRadius 0 True False (opp p2) path4
        path6 = arcCurveTo spec.outerRadius spec.outerRadius 0 True False p2 path5
        path7 = closePath path6
        path = path7
    in path.thePath
  else
    let deltaAngle = spec.endAngle - spec.startAngle
        ((x1, y1) as p1) = (spec.innerRadius * cos spec.startAngle, spec.innerRadius * sin spec.startAngle)
        ((x2, y2) as p2) = (spec.outerRadius * cos spec.startAngle, spec.outerRadius * sin spec.startAngle)
        ((x3, y3) as p3) = (spec.outerRadius * cos spec.endAngle, spec.outerRadius * sin spec.endAngle)
        ((x4, y4) as p4) = (spec.innerRadius * cos spec.endAngle, spec.innerRadius * sin spec.endAngle)
        lFlag = if deltaAngle >= pi then True else False
        path1 = moveTo p1 path0
        path2 = lineTo p2 path1
        path3 = arcCurveTo spec.outerRadius spec.outerRadius 0 lFlag True p3 path2
        path4 = lineTo p4 path3
        path5 = arcCurveTo spec.innerRadius spec.innerRadius 0 lFlag False p1 path4
        path6 = closePath path5
        path = path6
    in path.thePath

-- arcCurveTo radiusX radiusY rot largeFlag sweepFlag (x, y) path =
