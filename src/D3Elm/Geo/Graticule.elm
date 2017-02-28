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
{-}
closingMap : (a -> b) -> List a -> List b
closingMap f l =
  let go b0 =
        let go1 l =
          case l of
            [] -> [b0]
            (h::r) -> f h :: go1 r
          in go1
  in case l of
    [] -> []
    [a] -> [f a]
    (h::r) -> let b0 = f h in b0 :: go b0 r


mkList s e last n =
  let d = (e - s) / n
      go x0 n =
        if n == 0
        then last
        else x0 :: go (x0 + d) (n-1)
  in go s n

toPosition : (Float, Float) -> Position
toPosition (a, b) = (a, b, 0)
-}
graticule : GraticuleConf -> Geometry
graticule conf =
  let meridians = graticuleMedidians conf
      parallels = graticuleParallels conf
  in MultiLineString <| L.map (\l -> L.map toGeoPosition l) (meridians ++ parallels)

graticuleMedidians : GraticuleConf -> List (List (Float, Float))
graticuleMedidians conf =
  let lambdas = mkList 0 pi [] conf.nbMeridians
      phis = D.log "phis" <| mkList 0 tau [] conf.nbMeridianSteps
      meridians = L.map (\lambda -> closingMap (\phi -> (lambda, phi)) phis) lambdas
  in meridians


graticuleParallels : GraticuleConf -> List (List (Float, Float))
graticuleParallels conf =
  let lambdas = mkList (-pi) pi [] conf.nbParallelSteps
      dphi = pi/(conf.nbParallels + 1)
      phis = D.log "phis" <| mkList (-pi/2+dphi) (pi/2-dphi) [pi/2-dphi] (conf.nbParallels-1)
      parallels = L.map (\phi -> closingMap (\lambda -> (lambda, phi)) lambdas) phis
  in parallels



















