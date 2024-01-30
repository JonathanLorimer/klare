{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Klare.Data.Layout where

import Data.Proxy
import Graphics.Rendering.OpenGL
import Data.Kind
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign ( plusPtr, nullPtr )
import Foreign.Storable.Tuple ()
import Data.List.NonEmpty (NonEmpty(..))

class HasComponents (a :: Type -> Type) where
  numComponents :: NumComponents

instance HasComponents Vertex1 where
  numComponents = 1

instance HasComponents Vertex2 where
  numComponents = 2

instance HasComponents Vertex3 where
  numComponents = 3

instance HasComponents Vertex4 where
  numComponents = 4

instance HasComponents Color3 where
  numComponents = 3

instance HasComponents TexCoord2 where
  numComponents = 2

class HasDataType a where
  dataType :: DataType

instance HasDataType Float where
  dataType = Float

bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

descriptor 
  :: forall f a b 
  .  (HasComponents f, HasDataType a, Storable (f a)) 
  => Stride 
  -> Ptr b 
  -> VertexArrayDescriptor b
descriptor = VertexArrayDescriptor (numComponents @f) (dataType @a)

class Layout a where
  descriptors :: forall b . [VertexArrayDescriptor b]

instance 
  ( HasComponents f, HasDataType a, Storable (f a)
  , HasComponents g, HasDataType b, Storable (g b)
  ) => Layout (f a, g b) where
  descriptors = 
    let stride = fromIntegral $ sizeOf @(f a, g b) undefined 
        desc1 = descriptor @f @a stride (bufferOffset 0) 
        desc2 = descriptor @g @b stride (bufferOffset $ fromIntegral $ sizeOf @(f a) undefined)
    in [ desc1, desc2]

instance 
  ( HasComponents f, HasDataType a, Storable (f a)
  , HasComponents g, HasDataType b, Storable (g b)
  , HasComponents h, HasDataType c, Storable (h c)
  ) => Layout (f a, g b, h c) where
  descriptors = 
    let stride = fromIntegral $ sizeOf @(f a, g b, h c) undefined 
        desc1 = descriptor @f @a stride (bufferOffset 0) 
        desc2 = descriptor @g @b stride (bufferOffset $ fromIntegral $ sizeOf @(f a) undefined)
        desc3 = descriptor @h @c stride (bufferOffset $ fromIntegral $ sizeOf @(f a, g b) undefined)
    in [desc1, desc2, desc3]

instance 
  ( HasComponents f, HasDataType a, Storable (f a)
  , HasComponents g, HasDataType b, Storable (g b)
  , HasComponents h, HasDataType c, Storable (h c)
  , HasComponents i, HasDataType d, Storable (i d)
  ) => Layout (f a, g b, h c, i d) where
  descriptors = 
    let stride = fromIntegral $ sizeOf @(f a, g b, h c) undefined 
        desc1 = descriptor @f @a stride (bufferOffset 0) 
        desc2 = descriptor @g @b stride (bufferOffset $ fromIntegral $ sizeOf @(f a) undefined)
        desc3 = descriptor @h @c stride (bufferOffset $ fromIntegral $ sizeOf @(f a, g b) undefined)
        desc4 = descriptor @i @d stride (bufferOffset $ fromIntegral $ sizeOf @(f a, g b, h c) undefined)
    in [desc1, desc2, desc3, desc4]
