{-# LANGUAGE AllowAmbiguousTypes #-}
module Klare.Data.Buffer 
( BufferWitness
, bwObject
, bwData
, bwTarget
, registerBuffer
, withBuffer
, registerLayout
, withLayout
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL.GL as GL
import Foreign.Marshal.Array (withArray)
import Data.Semigroup (Sum(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Exception (bracket)
import Klare.Data.Layout
import Data.Traversable (forM)
import Data.Foldable (forM_)

data BufferWitness a = 
  BufferWitness 
    { bwObject :: !BufferObject 
    , bwData :: !a
    , bwTarget :: !BufferTarget
    }

registerBuffer :: Storable a => BufferTarget -> BufferUsage -> [a] -> IO (BufferWitness [a])
registerBuffer target usage bufferData = do
  bufferObject <- GL.genObjectName
  GL.bindBuffer target $= Just bufferObject

  withArray bufferData $ \ptr -> do
    let size = fromIntegral . getSum $ foldMap (Sum . sizeOf) bufferData
    GL.bufferData target $= (size, ptr, usage)

  pure $ BufferWitness 
    { bwObject = bufferObject
    , bwData = bufferData
    , bwTarget = target
    }
  
withBuffer 
  :: Storable a 
  => BufferTarget 
  -> BufferUsage 
  -> [a]
  -> (BufferWitness [a] -> IO ()) 
  -> IO () 
withBuffer target usage bufferData f =
  bracket
    (registerBuffer target usage bufferData)
    f 
    (\BufferWitness{..} -> do 
      GL.bindBuffer target $= Nothing
      GL.deleteObjectName bwObject
    )
    
registerLayout :: forall a. Layout a => IO [GL.AttribLocation]
registerLayout = 
  forM (zip (descriptors @a) [0..]) $ \(desc, idx) -> do
    let attribLoc = GL.AttribLocation idx
    GL.vertexAttribPointer attribLoc $= (GL.ToFloat, desc)
    GL.vertexAttribArray attribLoc $= GL.Enabled
    pure attribLoc

withLayout 
  :: forall a b . Layout a 
  => ([GL.AttribLocation] -> IO ()) 
  -> IO () 
withLayout f =
  bracket
    (registerLayout @a)
    f 
    (\locs -> forM_ locs $ 
        \loc -> GL.vertexAttribArray loc $= GL.Disabled
    )
