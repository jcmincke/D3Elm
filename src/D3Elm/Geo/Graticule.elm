module D3Elm.Geo.Graticule exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)

import GeoJson exposing (..)

import D3Elm.Geo.Rotation exposing (..)
import D3Elm.Geo.Merge exposing (..)
import D3Elm.Geo.Common exposing (..)
import D3Elm.Geo.Math as Math exposing (..)
import D3Elm.Geo.Scale exposing (..)

type alias GraticuleConf = {
  nbParallels : Float
  , nbMeridians : Float
  , nbParallelSteps : Float
  , nbMeridianSteps : Float
  }


graticule : GraticuleConf -> Geometry
graticule conf =
  let meridians = graticuleMedidians conf
      parallels = graticuleParallels conf
  in MultiLineString <| L.map (\l -> L.map toGeoPosition l) (meridians ++ parallels)

graticuleMedidians : GraticuleConf -> List (List (Float, Float))
graticuleMedidians conf =
  let lambdas = mkList (-pi) (pi) [] conf.nbMeridians
      phis = D.log "phis" <| mkList (-pi/2) (pi/2) [pi/2] conf.nbMeridianSteps
      meridians = L.map (\lambda -> L.map (\phi -> (lambda, phi)) phis) lambdas
  in meridians


graticuleParallels : GraticuleConf -> List (List (Float, Float))
graticuleParallels conf =
  let lambdas = mkList (-pi) pi [] conf.nbParallelSteps
      dphi = pi/(conf.nbParallels + 1)
      phis = D.log "phis" <| mkList (-pi/2+dphi) (pi/2-dphi) [pi/2-dphi] (conf.nbParallels-1)
      parallels = L.map (\phi -> closingMap (\lambda -> (lambda, phi)) lambdas) phis
  in parallels



type alias PolyGraticuleConf = {
  deltaLambda : Float
  , deltaPhi : Float
  , lambdaRepeats : List Float
  , phiRepeats : List Float
  , nbParallelSteps : Float
  , nbMeridianSteps : Float
  }


onePolygon conf =
  let lambdas = mkList 0 conf.deltaLambda [conf.deltaLambda] conf.nbParallelSteps
      phis = mkList -conf.deltaPhi conf.deltaPhi [conf.deltaPhi] (2 * conf.nbParallelSteps)
      p1s = L.map (\l -> (l, -conf.deltaPhi)) lambdas
      p2s = L.map (\p -> (conf.deltaLambda, p)) phis
      p3s = L.reverse <| L.map (\l -> (l, conf.deltaPhi)) lambdas
      p4s = L.reverse <| L.map (\p -> (0, p)) phis

  in p1s ++ p2s ++ p3s ++ p4s


polygonialGraticule : PolyGraticuleConf -> Geometry
polygonialGraticule conf =
  let polygon = onePolygon conf
      proc (lambda, phi) acc =
        let rotatedPolygon = L.map (rotate lambda 0 0) shiftedPolygon
            shiftedPolygon = L.map (translate 0 phi) polygon
        in rotatedPolygon :: acc
      polygons = L.foldl proc [] (L.concatMap (\l -> L.map (\p -> (l,p)) conf.phiRepeats) conf.lambdaRepeats)
      geo = MultiPolygon (L.map (\polygon -> [L.map toGeoPosition polygon]) polygons)
  in geo











