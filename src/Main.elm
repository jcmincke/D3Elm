module Main exposing (main)

import Html exposing (Html, button, div, text)

import D3Elm.Geo.Merge

import D3Elm.Hierarchy.Tree.Tree
import D3Elm.Hierarchy.Treemap.Treemap
import D3Elm.Path.Path
import D3Elm.Shapes.Sector
import D3Elm.Shapes.Pie

import D3Elm.Shapes.Curves.Basis
import D3Elm.Shapes.Curves.Linear
import D3Elm.Shapes.Curves.Step
import D3Elm.Shapes.Curves.Natural
import D3Elm.Shapes.Curves.Cardinal
import D3Elm.Shapes.Curves.Monotone

import D3Elm.Geo.Math
import D3Elm.Geo.Rotation
import D3Elm.Geo.Interpolate
import D3Elm.Geo.Graticule

import D3Elm.Geo.Transformation
import D3Elm.Geo.Projection.Stereographic
import D3Elm.Geo.Projection.Gnomonic
import D3Elm.Geo.Projection.Orthographic
import D3Elm.Geo.Rendering.Simple
import D3Elm.Geo.Clip.Circle
import D3Elm.Geo.Clip.Clip



main = text "This is ElmViz !"
