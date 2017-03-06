module ElmViz.Shapes.Curves.Linear exposing (..)

import Basics exposing (..)

import ElmViz.Path.Path exposing (..)



linear points path =
  case points of
    [] -> path
    (p0::[]) -> path
    (p0::r) ->
      let path1 = moveTo p0 path
          go points path =
            case points of
              [] -> path
              (p::r) ->
                let path1 = lineTo p path
                in go r path1
      in go r path1

