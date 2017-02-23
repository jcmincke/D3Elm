module D3Elm.Geo.Projection.Orthographic exposing (..)

import Basics as B exposing (..)

import D3Elm.Geo.Math as Math exposing (..)




orthographic (x, y) = (cos y * sin x , sin y)
