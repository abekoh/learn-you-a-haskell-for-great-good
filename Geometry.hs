module Geometry (sphereVolume) where

sphereVolume :: Float -> Float
sphereVolume radious = (4.0 / 3.0) * pi * (radious ^ 3)
