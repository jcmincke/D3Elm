module ElmViz.Geo.Projection.Orthographic exposing (..)

import Basics as B exposing (..)

import ElmViz.Geo.Math as Math exposing (..)




orthographic (x, y) = (cos y * sin x , sin y)
