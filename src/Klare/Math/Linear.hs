module Klare.Math.Linear where

import Linear

translate :: Num a => V3 a -> M44 a
translate (V3 x y z) = 
  V4
    (V4 1 0 0 x)
    (V4 0 1 0 y)
    (V4 0 0 1 z)
    (V4 0 0 0 1)

scale :: Num a => V3 a -> M44 a
scale (V3 x y z) = 
  V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 z 0)
    (V4 0 0 0 1)

rotate2D :: (Epsilon a, Floating a) => a -> M33 a
rotate2D theta = fromQuaternion $ axisAngle (V3 0 0 1) theta 
