module Klare.Font where

import Graphics.Rendering.OpenGL
import FreeType
import Foreign
import Linear

data CharGlyph = 
  CharGlyph
    { charGlyphTextureObjectt :: TextureObject
    , charGlyphSize :: V2 Word32
    , charGlyphBearing :: V2 Int32
    , charGlyphAdvance :: FT_Vector
    }

registerGlyph :: FT_Face -> Char -> IO CharGlyph
registerGlyph ff c = do
  ft_Load_Char ff (fromIntegral $ fromEnum c) FT_LOAD_RENDER

  texObj <- genObjectName
  textureBinding Texture2D $= Just texObj

  face <- peek ff
  glyph <- peek $ frGlyph face
  let FT_Bitmap{..} = gsrBitmap glyph
      texSize = TextureSize2D (fromIntegral bWidth) (fromIntegral bRows) 

  texImage2D Texture2D NoProxy 0 R8 texSize 0 $ 
    PixelData Red UnsignedByte bBuffer

  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  
  pure $ 
    CharGlyph
      { charGlyphTextureObjectt = texObj
      , charGlyphSize = V2 bWidth bRows
      , charGlyphBearing = V2 (gsrBitmap_left glyph) (gsrBitmap_top glyph)
      , charGlyphAdvance = gsrAdvance glyph
      }
