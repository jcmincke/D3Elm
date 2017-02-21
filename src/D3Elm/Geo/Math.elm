module D3Elm.Geo.Math exposing (..)

import Basics exposing (..)

epsilon = 1e-6
epsilon2 = 1e-12
halfPi = pi / 2
quarterPi = pi / 4
tau = pi * 2
degrees = 180 / pi
radians = pi / 180

--abs = Basics.abs
--atan = Basics.atan
--atan2 = Basics.atan2
--cos = Basics.cos
ceil = Basics.ceiling
exp x = Basics.e ^ x
floor = Basics.floor
log = Basics.logBase Basics.e
pow x y = x ^ y
--sin = Basics.sin
--sqrt = Basics.sqrt
--tan = Basics.tan


sign : Float -> Float
sign x =  if x > 0
          then 1
          else  if x < 0
                then -1
                else 0

acos : Float -> Float
acos x  =
  if x > 1
  then 0
  else  if x < -1
        then pi
        else Basics.acos x

asin : Float -> Float
asin x  =
  if x > 1
  then halfPi
  else  if x < -(halfPi)
        then pi
        else Basics.asin x

haversin : Float -> Float
haversin x =
  let a = sin (x / 2)
  in a * a
