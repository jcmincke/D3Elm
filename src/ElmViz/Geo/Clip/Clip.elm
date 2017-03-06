
module ElmViz.Geo.Clip.Clip exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)
import Maybe as M exposing (..)

import GeoJson exposing (..)

import ElmViz.Common exposing (..)
import ElmViz.Geo.Cartesian exposing (..)
import ElmViz.Geo.Length exposing (..)
import ElmViz.Geo.Math exposing (..)
import ElmViz.Geo.Clip.Circle exposing (..)


type alias ClippingTransformation = GeoJsonObject -> Maybe GeoJsonObject

createCircleClippingTransformation : Float -> ClippingTransformation
createCircleClippingTransformation radiusAngle geo =
  let geo1 =
    case geo of
      Geometry geometry -> M.map Geometry (goGeometry radiusAngle geometry)
      Feature o -> Just <| Feature (goFeatureObject radiusAngle o)
      FeatureCollection l -> Just <| FeatureCollection (L.map (goFeatureObject radiusAngle) l)
  in geo1


goFeatureObject : Float -> FeatureObject -> FeatureObject
goFeatureObject radiusAngle o =
  case o.geometry of
    Just g -> { o | geometry = goGeometry radiusAngle g}
    Nothing -> o

goGeometry : Float -> Geometry -> Maybe Geometry
goGeometry radiusAngle geometry =
  let toPosition l = L.map (\(x, y) -> (x, y, 0)) l
      fromPosition l = L.map (\(x, y, _) -> (x, y)) l
  in  case geometry of
      Point (lambda, phi, _) -> if circlePointClipping radiusAngle (lambda, phi)
                                then Just (Point (lambda, phi, 0))
                                else Nothing
      MultiPoint l -> Just (MultiPoint (toPosition <| L.filter (circlePointClipping radiusAngle) (fromPosition l)))
      LineString l ->
        case circleLineClipping radiusAngle (fromPosition l) of
        [] -> Nothing
        [l1] -> Just (LineString <| toPosition l1)
        lines -> Just (MultiLineString <| L.map toPosition lines)
      MultiLineString l ->
        case L.concatMap (\il -> circleLineClipping radiusAngle (fromPosition il)) l of
          [] -> Nothing
          l1 -> Just (MultiLineString <| L.map toPosition l1)
      -- todo: implements
      Polygon l -> Just (Polygon l)
      MultiPolygon l -> Just (MultiPolygon l)
      GeometryCollection l -> Just (GeometryCollection l)

