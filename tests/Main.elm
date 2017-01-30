port module Main exposing (..)

import Test exposing (..)


import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import TestD3Elm.D3Elm.Hierarchy.Tree.Tree as Tree
import TestD3Elm.D3Elm.Hierarchy.Cluster as Cluster
import TestD3Elm.D3Elm.Hierarchy.Treemap.Treemap as Treemap
allTests : Test
allTests =
  describe "D3" [
    Tree.all
    , Cluster.all
    , Treemap.all
    ]

main : TestProgram
main =
    run emit allTests -- Tree.all


port emit : ( String, Value ) -> Cmd msg
