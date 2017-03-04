module D3Elm.Geo.Graticule exposing (..)

import Debug as D exposing (..)
import List as L exposing (..)

import GeoJson exposing (..)

import D3Elm.Geo.Common exposing (..)
import D3Elm.Geo.Math as Math exposing (..)

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



















