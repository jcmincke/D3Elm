module D3Elm.Geo.Transformation exposing (..)

import Basics as B exposing (..)
import List as L exposing (..)

import D3Elm.Geo.Math as Math exposing (..)
import GeoJson exposing (..)


type alias Transformation = GeoJsonObject -> GeoJsonObject

createTransformation : ((Float, Float) -> (Float, Float)) -> Transformation
createTransformation f geo =
  let geo1 =
    case geo of
      Geometry geometry -> Geometry (goGeometry f geometry)
      Feature o -> Feature (goFeatureObject f o)
      FeatureCollection l -> FeatureCollection (L.map (goFeatureObject f) l)
  in geo1




goFeatureObject f o =
  case o.geometry of
    Just g -> { o | geometry = Just (goGeometry f g)}
    Nothing -> o

goGeometry : ((Float, Float) -> (Float, Float)) -> Geometry -> Geometry
goGeometry f geometry =
  let transformP = transformPosition f
  in  case geometry of
      Point p -> Point (transformP p)
      MultiPoint l -> MultiPoint (L.map transformP  l)
      LineString l -> LineString (L.map transformP  l)
      MultiLineString l -> MultiLineString (L.map (\il -> L.map transformP il) l)
      Polygon l -> Polygon (L.map (\il -> L.map transformP il) l)
      MultiPolygon l -> MultiPolygon (L.map (\ol -> L.map (\il -> L.map transformP il) ol) l)
      GeometryCollection l -> GeometryCollection (L.map (goGeometry f) l)

transformPosition : ((Float, Float) -> (Float, Float)) -> Position -> Position
transformPosition f  (a,b,c) =
  let (a1, b1) = f (a, b)
  in (a1, b1, c)
