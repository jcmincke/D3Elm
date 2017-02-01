module D3Elm.Path.Path exposing (..)

import Basics exposing (..)

tau = 2 * pi
epsilon = 1e-6
tauEpsilon = tau - epsilon


type alias Path = {
  previousX : Float
  , previousY : Float
  , currentX : Float
  , currentY : Float
  , thePath : String
}

path0 : Path
path0 = {
  previousX = 0.0
  , previousY = 0.0
  , currentX = 0.0
  , currentY = 0.0
  , thePath = ""
  }

resetFrom : Path -> Path
resetFrom path =
    { path | thePath = "" }

moveTo : (Float, Float) -> Path -> Path
moveTo (x, y) path =
  { path | previousX = path.currentX, previousY = path.currentY
          , currentX = x, currentY = y
          , thePath = path.thePath ++ " M " ++ toString x ++ toString y
  }

relMoveTo : (Float, Float) -> Path -> Path
relMoveTo (dx, dy) path =
  { path | previousX = path.currentX, previousY = path.currentY
          , currentX = path.currentX + dx, currentY =  path.currentY + dy
          , thePath = path.thePath ++ " m " ++ toString dx ++ toString dy
  }

closePath : Path -> Path
closePath path =
  { path | thePath = path.thePath ++ " Z "}


lineTo : (Float, Float) -> Path -> Path
lineTo (x, y) path =
  { path | previousX = path.currentX, previousY = path.currentX
           , currentX = x, currentY = y
           , thePath = path.thePath ++ " L " ++ toString x ++ toString y
  }

relLineTo : (Float, Float) -> Path -> Path
relLineTo (dx, dy) path =
  let nx = path.currentX + dx
      ny = path.currentY + dy
  in  { path | previousX = path.currentX, previousY = path.currentX
               , currentX = nx, currentY = nx
               , thePath = path.thePath ++ " l " ++ toString dx ++ toString dy
      }



hLineTo : Float -> Path -> Path
hLineTo x path =
  { path | previousX = path.currentX, previousY = path.currentX
           , currentX = x
           , thePath = path.thePath ++ " H " ++ toString x
  }

relHLineTo : Float -> Path -> Path
relHLineTo d path =
  { path | previousX = path.currentX, previousY = path.currentX
           , currentX = path.currentX + d
           , thePath = path.thePath ++ " h " ++ toString d
  }


vLineTo : Float -> Path -> Path
vLineTo y path =
  { path | previousX = path.currentX, previousY = path.currentX
           , currentY = y
           , thePath = path.thePath ++ " V " ++ toString y
  }

relVLineTo : Float -> Path -> Path
relVLineTo d path =
  { path | previousX = path.currentX, previousY = path.currentX
           , currentY = path.currentY + d
           , thePath = path.thePath ++ " v " ++ toString d
  }

quadraticCurveTo : ((Float, Float), (Float, Float)) -> Path -> Path
quadraticCurveTo ((x1, y1), (x2, y2)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = x2, currentY = y2
           , thePath = path.thePath ++ " Q " ++ toString x1 ++ "," ++ toString y1
                                             ++ toString x2 ++ "," ++ toString y2
  }

relQuadraticCurveTo : ((Float, Float), (Float, Float)) -> Path -> Path
relQuadraticCurveTo ((dx1, dy1), (dx2, dy2)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = path.currentX + dx2, currentY = path.currentY + dy2
           , thePath = path.thePath ++ " q " ++ toString dx1 ++ "," ++ toString dy1
                                             ++ toString dx2 ++ "," ++ toString dy2
  }

quadraticReflexiveCurveTo : (Float, Float) -> Path -> Path
quadraticReflexiveCurveTo (x1, y1) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = x1, currentY = y1
           , thePath = path.thePath ++ " T " ++ toString x1 ++ "," ++ toString y1
  }


relQuadraticReflexiveCurveTo : (Float, Float) -> Path -> Path
relQuadraticReflexiveCurveTo (dx1, dy1) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = path.currentX + dx1, currentY = path.currentY + dy1
           , thePath = path.thePath ++ " t " ++ toString dx1 ++ "," ++ toString dy1
  }


cubicCurveTo : ((Float, Float), (Float, Float), (Float, Float)) -> Path -> Path
cubicCurveTo ((x1, y1), (x2, y2), (x3, y3)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = x3, currentY = y3
           , thePath = path.thePath ++ " C " ++ toString x1 ++ "," ++ toString y1
                                             ++ toString x2 ++ "," ++ toString y2
                                             ++ toString x3 ++ "," ++ toString y3
  }



relCubicCurveTo : ((Float, Float), (Float, Float), (Float, Float)) -> Path -> Path
relCubicCurveTo ((dx1, dy1), (dx2, dy2), (dx3, dy3)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = path.currentX + dx3, currentY = path.currentY + dy3
           , thePath = path.thePath ++ " c " ++ toString dx1 ++ "," ++ toString dy1
                                             ++ toString dx2 ++ "," ++ toString dy2
                                             ++ toString dx3 ++ "," ++ toString dy3
  }





cubicReflexiveCurveTo : ((Float, Float), (Float, Float)) -> Path -> Path
cubicReflexiveCurveTo ((x1, y1), (x2, y2)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = x2, currentY = y2
           , thePath = path.thePath ++ " S " ++ toString x1 ++ "," ++ toString y1
                                             ++ toString x2 ++ "," ++ toString y2
  }



relCubicReflexiveCurveTo : ((Float, Float), (Float, Float)) -> Path -> Path
relCubicReflexiveCurveTo ((dx1, dy1), (dx2, dy2)) path =
  { path | previousX = path.currentX, previousY = path.currentY
           , currentX = path.currentX + dx2, currentY = path.currentY + dy2
           , thePath = path.thePath ++ " s " ++ toString dx1 ++ "," ++ toString dy1
                                             ++ toString dx2 ++ "," ++ toString dy2
  }



arcCurveTo radiusX radiusY rot largeFlag sweepFlag (x, y) path =
  let lFlagBool = if largeFlag then "1" else 0
  let sFlagBool = if sweepFlag then "1" else 0
  in  { path |  previousX = path.currentX, previousY = path.currentY
                , currentX = x, currentY = y
                , thePath = path.thePath ++ " A " ++
                  toString radiusX ++ "," ++ toString radiusY ++
                  toString rot ++ "," ++ lFlagBool ++ sFlagBool ++
                  toString x ++ " , " ++ toString y
      }




relArcCurveTo radiusX radiusY rot largeFlag sweepFlag (dx, dy) path =
  let lFlagBool = if largeFlag then "1" else 0
  let sFlagBool = if sweepFlag then "1" else 0
  in  { path |  previousX = path.currentX, previousY = path.currentY
                , currentX = path.currentX + dx, currentY = path.currentY + dy
                , thePath = path.thePath ++ " a " ++
                  toString radiusX ++ "," ++ toString radiusY ++
                  toString rot ++ "," ++ lFlagBool ++ sFlagBool ++
                  toString dx ++ " , " ++ toString dy
      }







