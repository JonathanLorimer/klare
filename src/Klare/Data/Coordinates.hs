{-# LANGUAGE DeriveFunctor #-}
module Klare.Data.Coordinates where

import Linear

data CoordinateObject a = CoordinateObject
  { localSpace :: a
  -- ^ The object or objects that exist in local space and needs to be transformed into our scene
  , model :: M44 Float
  -- ^ The matrix that transforms local space to world space
  , view :: M44 Float
  -- ^ The matrix that transforms world space to camera space
  , projection :: M44 Float
  -- ^ The matrix that converts from our coordinate structure to normalized device coordinates
  } deriving (Eq, Ord, Show, Functor)

compose :: (M44 Float -> a -> b) -> CoordinateObject a -> b
compose f CoordinateObject{..} = (projection !*! view !*! model) `f` localSpace

