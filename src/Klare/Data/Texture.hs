module Klare.Data.Texture where

import Control.Exception (bracket)
import Control.Exception.Base (bracket_)
import Data.Traversable (for)
import Graphics.GLUtil.Textures qualified as GLUtil
import Graphics.Rendering.OpenGL

{- | If not used with an active texture unit, or as a part of 'registerTextures' will use the default texture unit
which is 0
-}
registerTexture :: (BindableTextureTarget t, ParameterizedTextureTarget t) => t -> IO TextureObject -> IO ()
registerTexture texTarget texObjectIO = do
  texObject <- texObjectIO
  textureFilter texTarget $= ((Linear', Nothing), Linear')
  GLUtil.texture2DWrap $= (Repeated, ClampToBorder)
  textureBinding texTarget $= Just texObject
  texture texTarget $= Enabled

registerTextures ::
  (BindableTextureTarget t, ParameterizedTextureTarget t) =>
  [(t, IO TextureObject)] ->
  IO [TextureUnit]
registerTextures objs =
  for (zip objs [0 ..]) $ \(texData, unit) -> do
    activeTexture $= TextureUnit unit
    uncurry registerTexture texData
    pure $ TextureUnit unit
