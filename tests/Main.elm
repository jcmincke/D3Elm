port module Main exposing (..)

import Test exposing (..)


import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import TestElmViz.ElmViz.Hierarchy.Tree.Tree as Tree
import TestElmViz.ElmViz.Hierarchy.Cluster as Cluster
import TestElmViz.ElmViz.Hierarchy.Treemap.Treemap as Treemap
import TestElmViz.ElmViz.Shapes.Curves.Curves as Curves
import TestElmViz.ElmViz.Voronoi.Voronoi as Voronoi
import TestElmViz.ElmViz.Voronoi.Common as VoronoiCommon
import TestElmViz.ElmViz.Geo.Rotation as Rotation
import TestElmViz.ElmViz.Geo.Graticule as Graticule
import TestElmViz.ElmViz.Geo.Clip.Circle as Circle
import TestElmViz.ElmViz.Geo.Clip.Circle1 as Circle1

import TestElmViz.ElmViz.Misc.Test as Misc

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
    , Circle.all
    , Circle1.all
    ]

main : TestProgram
main =
    run emit allTests -- Tree.all


port emit : ( String, Value ) -> Cmd msg
