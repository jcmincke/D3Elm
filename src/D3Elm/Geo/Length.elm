

module D3Elm.Geo.Math exposing (..)


orthodromicLength (lambda0, phi0) (lambda1, phi1) =
  let sinPhi = sin phi1
      cosPhi = cos phi1
      delta = abs(lambda1 - lambda0)
      cosDelta = cos delta
      sinDelta = sin delta
      x = cosPhi * sinDelta
      y = cosPhi0 * sinPhi - sinPhi0 * cosPhi * cosDelta
      z = sinPhi0 * sinPhi + cosPhi0 * cosPhi * cosDelta
  in  atan2(sqrt(x * x + y * y), z)






















