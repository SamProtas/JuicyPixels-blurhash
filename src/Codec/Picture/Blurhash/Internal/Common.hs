-- |
-- Module: Codec.Picture.Blurhash.Internal.Common
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal Blurhash helper functions shared by encoding and decoding.
--
-- __Note__: This is an internal module not subject to PVP adherence.
module Codec.Picture.Blurhash.Internal.Common where

import Codec.Picture (Image, PixelRGB8(..), PixelRGBF(..), pixelMap)

-- | Helper function specified by other Blurhash implementations
signPow :: Float -> Float -> Float
signPow v ex = fixSign $ (abs v) ** ex
  where
    fixSign = if v < 0 then ((-1)*) else id    

-- | Clamps it's 3rd argument between the min and max specified by the 1st and 2nd arguments
--   respectively
clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal = min maxVal . max minVal

-- | Convert an RGB8 Image to an RGBF Image using the color conversion specified by Blurhash
sRGBImageToLinear :: Image PixelRGB8 -> Image PixelRGBF
sRGBImageToLinear = pixelMap pixelToLinear

-- | Convert an RGB8 Pixel to an RGBF Pixel using the color conversion specified by Blurhash
pixelToLinear :: PixelRGB8 -> PixelRGBF
pixelToLinear (PixelRGB8 r g b) = PixelRGBF (toLinear r) (toLinear g) (toLinear b)
  where
    toLinear c = let v = realToFrac c / 255
                 in if v < 0.04045
                    then v / 12.92
                    else ((v + 0.055) / 1.055) ** 2.4

-- | Convert an RGBF Image to an RGB8 Image using the color conversion specified by Blurhash
linearImageToSRGB :: Image PixelRGBF -> Image PixelRGB8
linearImageToSRGB = pixelMap linearPixelToSRGB

-- | Convert an RGBF Pixel to an RGB8 Pixel using the color conversion specified by Blurhash
linearPixelToSRGB :: PixelRGBF -> PixelRGB8
linearPixelToSRGB (PixelRGBF r g b) = PixelRGB8 (linearToSRGB r) (linearToSRGB g) (linearToSRGB b)

-- | Convert an RGBF color to an RGB8 color using the color conversion specified by Blurhash
linearToSRGB :: (RealFrac a, Integral b, Floating a) => a -> b
linearToSRGB p = 
  floor $ if v < 0.0031308
          then v * 12.92 * 255 + 0.5
          else (1.055 * (v ** (1 / 2.4)) - 0.055) * 255 + 0.5
  where
    v = clamp 0 1 p

