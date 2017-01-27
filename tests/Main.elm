port module Main exposing (..)

import Tests2.D3Elm.Hierarchy.Tree.Tree as Tree
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Tree.all


port emit : ( String, Value ) -> Cmd msg
