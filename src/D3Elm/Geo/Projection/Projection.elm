module D3Elm.Geo.Projection.Projection exposing (..)

import Basics as B exposing (..)

import GeoJson exposing (..)

import D3Elm.Geo.Math as Math exposing (..)
import D3Elm.Geo.Transformation exposing (..)



orthographic (x, y) = (cos y * sin x , sin y)

