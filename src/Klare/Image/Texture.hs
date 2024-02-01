{-# LANGUAGE RankNTypes #-}
module Klare.Image.Texture where

import Codec.Picture
import Graphics.GLUtil
import Codec.Picture.Types
import Codec.Picture.Extra (flipVertically)
import Data.Functor ((<&>))

readTexInfo :: FilePath
            -> (forall a. Pixel a => Image a -> Image a)
            -> (forall a. IsPixelData a => TexInfo a -> IO b)
            -> IO (Either String b)
readTexInfo f transform k = do 
  image <- readImage f 
  print $ image <&> \case
    (ImageY8 (Image w h p)) -> "ImageY8"
    (ImageYF (Image w h p)) -> "ImageYF"
    (ImageYA8 _) -> "ImageYA8"
    (ImageRGB8 _) -> "ImageRGB8"
    (ImageRGBF _) -> "ImageRGBF"
    (ImageRGBA8 _) -> "ImageRGBA8"
    (ImageYCbCr8 img) -> "ImageYCbCr8"
    _ -> "Unsupported image format"
  either (return . Left) (aux . dynamicPixelMap transform)  image
  where aux (ImageY8 (Image w h p)) = Right <$> k (texInfo w h TexMono p)
        aux (ImageYF (Image w h p)) = Right <$> k (texInfo w h TexMono p)
        aux (ImageYA8 _) = return $ Left "YA format not supported"
        aux (ImageRGB8 (Image w h p)) = Right <$> k (texInfo w h TexRGB p)
        aux (ImageRGBF (Image w h p)) = Right <$> k (texInfo w h TexRGB p)
        aux (ImageRGBA8 (Image w h p)) = Right <$> k (texInfo w h TexRGBA p)
        aux (ImageYCbCr8 img) = aux . ImageRGB8 $ convertImage img
        aux _ = return $ Left "Unsupported image format"

readTexInfoFlipped :: FilePath
                   -> (forall a. IsPixelData a => TexInfo a -> IO b)
                   -> IO (Either String b)
readTexInfoFlipped f = readTexInfo f flipVertically
