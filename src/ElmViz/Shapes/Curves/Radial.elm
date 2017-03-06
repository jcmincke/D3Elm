module ElmViz.Shapes.Curves.Radial exposing (..)

import Basics exposing (..)
import List as L exposing(..)

import ElmViz.Path.Path exposing (..)

import List as L exposing (..)
import Tuple  exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Shapes.Curves.Linear exposing (..)

import Basics exposing (..)



radial points path =
  let points1 = L.map (\(r, a) -> (r * sin a, (-1) * r * cos a)) points
  in linear points1 path


