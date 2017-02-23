module D3Elm.Geo.Rendering.Simple exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)

import GeoJson exposing (..)

import D3Elm.Path.Path exposing (..)
import D3Elm.Geo.Transformation exposing (..)
import D3Elm.Geo.Projection.Orthographic exposing (..)
import D3Elm.Geo.Math as Math exposing (..)


type alias SimpleRenderContext a = {
  renderPoint : (Float, Float) -> a -> a
  , renderLine : List (Float, Float) -> a -> a
  }


render : SimpleRenderContext a -> GeoJson -> a -> a
render ctx (geo, _) a =
    case geo of
      Geometry geometry -> goGeometry ctx geometry a
      Feature o -> goFeatureObject ctx o a
      FeatureCollection l -> L.foldl (\e a1 -> goFeatureObject ctx  e a1) a l


goFeatureObject : SimpleRenderContext a -> FeatureObject -> a -> a
goFeatureObject ctx o a =
  case o.geometry of
    Just g -> goGeometry ctx g a
    Nothing -> a

goGeometry : SimpleRenderContext a -> Geometry -> a -> a
goGeometry ctx geometry a =
  let getxy (x, y, _) = (x, y)
  in  case geometry of
      Point p -> ctx.renderPoint (getxy p) a
      MultiPoint l -> L.foldl (\p a1 -> ctx.renderPoint (getxy p) a1) a l
      LineString l -> ctx.renderLine (L.map getxy l) a
      MultiLineString l -> L.foldl (\l a1 -> ctx.renderLine (L.map getxy l) a1) a l
      Polygon l -> L.foldl (\l a1 -> ctx.renderLine (L.map getxy l) a1) a l
      MultiPolygon l ->  L.foldl (\l1 a1 -> L.foldl (\l2 a2 -> ctx.renderLine (L.map getxy l2) a2) a1 l1) a l
      GeometryCollection l -> L.foldl (\g a1 -> goGeometry ctx g a1) a l
















