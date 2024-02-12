{-# LANGUAGE DeriveFunctor #-}
module Klare.Example.Geometry where

data Faces3D a = X a | Y a | Z a
  deriving (Eq, Ord, Show)

data Vertices3D a = 
  Vertices3D
    { x :: a
    , y :: a
    , z :: a
    } deriving (Eq, Ord, Show, Functor)

cubeVertices :: [Vertices3D Bool]
cubeVertices = do
  x <- [False ..]
  y <- [False ..]
  z <- [False ..]
  pure Vertices3D{..}

cubeFaces :: [Faces3D Bool]
cubeFaces = do 
  b <- [False ..]
  [X b, Y b, Z b]

