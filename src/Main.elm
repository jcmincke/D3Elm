module Main exposing (main)

import Html exposing (Html, button, div, text)

import ElmViz.Geo.Merge

import ElmViz.Hierarchy.Tree.Tree
import ElmViz.Hierarchy.Treemap.Treemap
import ElmViz.Path.Path
import ElmViz.Shapes.Sector
import ElmViz.Shapes.Pie

import ElmViz.Shapes.Curves.Basis
import ElmViz.Shapes.Curves.Linear
import ElmViz.Shapes.Curves.Step
import ElmViz.Shapes.Curves.Natural
import ElmViz.Shapes.Curves.Cardinal
import ElmViz.Shapes.Curves.Monotone

import ElmViz.Geo.Math
import ElmViz.Geo.Rotation
import ElmViz.Geo.Interpolate
import ElmViz.Geo.Graticule

import ElmViz.Geo.Transformation
import ElmViz.Geo.Projection.Stereographic
import ElmViz.Geo.Projection.Gnomonic
import ElmViz.Geo.Projection.Orthographic
import ElmViz.Geo.Rendering.Simple
import ElmViz.Geo.Clip.Circle
import ElmViz.Geo.Clip.Clip



main = text "This is ElmViz !"
