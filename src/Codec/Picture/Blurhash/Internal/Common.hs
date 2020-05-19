{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Picture.Blurhash.Internal.Common where

import Codec.Picture


signPow :: Float -> Float -> Float
signPow v ex = fixSign $ (abs v) ** ex
  where
    fixSign = if v < 0 then ((-1)*) else id    

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal = min maxVal . max minVal


sRGBImageToLinear :: Image PixelRGB8 -> Image PixelRGBF
sRGBImageToLinear = pixelMap pixelToLinear

pixelToLinear :: PixelRGB8 -> PixelRGBF
pixelToLinear (PixelRGB8 r g b) = PixelRGBF (toLinear r) (toLinear g) (toLinear b)
  where
    toLinear c = let v = realToFrac c / 255
                 in if v < 0.04045
                    then v / 12.92
                    else ((v + 0.055) / 1.055) ** 2.4

                         
  
linearImageToSRGB :: Image PixelRGBF -> Image PixelRGB8
linearImageToSRGB = pixelMap linearPixelToSRGB

linearPixelToSRGB :: PixelRGBF -> PixelRGB8
linearPixelToSRGB (PixelRGBF r g b) = PixelRGB8 (linearToSRGB r) (linearToSRGB g) (linearToSRGB b)

linearToSRGB :: (RealFrac a, Integral b, Floating a) => a -> b
linearToSRGB p = 
  floor $ if v < 0.0031308
          then v * 12.92 * 255 + 0.5
          else (1.055 * (v ** (1 / 2.4)) - 0.055) * 255 + 0.5
  where
    v = clamp 0 1 p

