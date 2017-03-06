module ElmViz.Geo.Merge exposing (..)

import Basics as B exposing (..)
import List as L exposing (..)
import Json.Encode as Json exposing (null)

import ElmViz.Geo.Math as Math exposing (..)
import GeoJson exposing (..)


type alias Transformation = GeoJsonObject -> GeoJsonObject

mergeGeoJsonObject : GeoJsonObject -> GeoJsonObject -> GeoJsonObject
mergeGeoJsonObject geo1 geo2 =
  case (geo1, geo2) of
    (Geometry geometry1, Geometry geometry2)  -> Geometry (GeometryCollection [geometry1, geometry2])
    (Geometry geometry1, Feature fo)  ->
      let fo1 = {geometry = Just geometry1, properties = Json.null, id=Nothing}
      in FeatureCollection [fo1, fo]
    (Geometry geometry1, FeatureCollection l)  ->
      let fo1 = {geometry = Just geometry1, properties = Json.null, id=Nothing}
      in FeatureCollection (fo1::l)
    (Feature _, Geometry _)  -> mergeGeoJsonObject geo2 geo1
    (Feature fo1, Feature fo2) -> FeatureCollection [fo1, fo2]
    (Feature fo1, FeatureCollection l) -> FeatureCollection (fo1::l)
    (FeatureCollection _, Geometry _)  -> mergeGeoJsonObject geo2 geo1
    (FeatureCollection _, Feature _)  -> mergeGeoJsonObject geo2 geo1
    (FeatureCollection l1, FeatureCollection l2)  -> FeatureCollection (l1++l2)




