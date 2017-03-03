port module Main exposing (..)

import Test exposing (..)


import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import TestD3Elm.D3Elm.Hierarchy.Tree.Tree as Tree
import TestD3Elm.D3Elm.Hierarchy.Cluster as Cluster
import TestD3Elm.D3Elm.Hierarchy.Treemap.Treemap as Treemap
import TestD3Elm.D3Elm.Shapes.Curves.Curves as Curves
import TestD3Elm.D3Elm.Voronoi.Voronoi as Voronoi
import TestD3Elm.D3Elm.Voronoi.Common as VoronoiCommon
import TestD3Elm.D3Elm.Geo.Rotation as Rotation
import TestD3Elm.D3Elm.Geo.Graticule as Graticule
import TestD3Elm.D3Elm.Misc.Test as Misc

allTests : Test
allTests =
  describe "D3" [
    Tree.all
    , Cluster.all
    , Treemap.all
    , Curves.all
    , VoronoiCommon.all
    , Voronoi.all
    , Rotation.all
    , Graticule.all
    , Misc.all
    ]

main : TestProgram
main =
    run emit allTests -- Tree.all


port emit : ( String, Value ) -> Cmd msg
