{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Codec.Picture.Blurhash.Internal.Decode
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal Blurhash decoding implementation.
--
-- __Note__: This is an internal module not subject to PVP adherence.
module Codec.Picture.Blurhash.Internal.Decode where

import Control.Monad (when)
import qualified Data.Bits as Bits
import Data.Foldable (foldrM, foldl')
import Data.Word (Word8)
import GHC.Generics (Generic)

import Codec.Picture (Image, PixelRGB8(..), PixelRGBF(..), generateImage)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Codec.Picture.Blurhash.Internal.Base83
import Codec.Picture.Blurhash.Internal.Common

-- | Configuration for how to decode a blurhash to an image.
--
-- >>> let myDecodeConfig = decodeConfigDefault { punch = 1.1, outputWidth = 64, outputHeight = 64}
--
data DecodeConfig = DecodeConfig
  { punch :: Float
  -- ^ Adjusts the contrast of the decoded image. Larger values mean more contrast.
  --
  -- See 'DecodeConfig' for example use with record update syntax.
  , outputWidth :: Int
  -- ^ Output image pixel width.
  --
  -- See 'DecodeConfig' for example use with record update syntax.
  , outputHeight :: Int
  -- ^ Output image pixel height.
  --
  -- See 'DecodeConfig' for example use with record update syntax.
  } deriving (Show, Generic)

-- | Decoding error types.
data DecodeError
  = InvalidCharacterError Word8 -- ^ The provided blurhash included an un-decodable byte.
  | InvalidHashLength -- ^ The provided blurhash length was wrong.
  deriving (Show, Generic)


-- | A reasonable default configuration for decoding.
--
-- >>> punch decodeConfigDefault == 1
-- True
--
-- >>> outputWidth decodeConfigDefault == 32
-- True
--
-- >>> outputHeight decodeConfigDefault == 32
-- True
--
decodeConfigDefault :: DecodeConfig
decodeConfigDefault = DecodeConfig 1 32 32

-- | Decode a blurhash into an 'Image' 'PixelRGB8'. Calls 'decodeRGB8WithConfig' with 'decodeConfigDefault'.
-- 
--   When in doubt, use this function to decode a blurhash.
decodeRGB8
  :: BS.ByteString -- ^ The blurhash
  -> Either DecodeError (Image PixelRGB8)
decodeRGB8 = decodeRGB8WithConfig decodeConfigDefault

-- | Decode a blurhash into an 'Image' 'PixelRGB8' given a 'DecodeConfig'
decodeRGB8WithConfig
  :: DecodeConfig
  -> BS.ByteString -- ^ The blurhash.
  -> Either DecodeError (Image PixelRGB8)
decodeRGB8WithConfig config blurhash
  = linearImageToSRGB <$> decodeLinearWithConfig config blurhash

-- | Decode a blurhash into an 'Image' 'PixelRGBF'. Calls 'decodeLinearWithConfig' with 'decodeConfigDefault'.
--
-- Note: Blurhash implementations use a non-naive 'PixelRGBF' to 'PixelRGB8' conversion. If your
-- ultimate goal is to end up with an 'Image' 'PixelRGB8', be careful using this function and
-- scaling pixels by 255 as you will get different results.
decodeLinear
  :: BS.ByteString -- ^ The blurhash
  -> Either DecodeError (Image PixelRGBF)
decodeLinear = decodeLinearWithConfig decodeConfigDefault

-- | Decode a blurhash into an 'Image' 'PixelRGBF' given a 'DecodeConfig'.
--
-- Note: Blurhash implementations use a non-naive 'PixelRGBF' to 'PixelRGB8' conversion. If your
-- ultimate goal is to end up with an 'Image' 'PixelRGB8', be careful using this function and
-- scaling pixels by 255 as you will get different results.
decodeLinearWithConfig
  :: DecodeConfig
  -> BS.ByteString -- ^ The blurhash
  -> Either DecodeError (Image PixelRGBF)
decodeLinearWithConfig config hash = do
  let (sizeSection, lessSize) = BS.splitAt 1 hash
      (quantMaxSection, lessQuantMax) = BS.splitAt 1 lessSize
      dcSection = BS.take 4 lessQuantMax
  when
    (BS.null sizeSection || BS.null quantMaxSection || BS.length dcSection < 4)
    (Left InvalidHashLength)

  sizeInfo <- base83DecodeTagged sizeSection
  quantMaxVal <- base83DecodeTagged quantMaxSection
  dcValue <- base83DecodeTagged dcSection

  let sizeY = floor (realToFrac sizeInfo / 9 :: Float) + 1
      sizeX = (sizeInfo `mod` 9) + 1
      realMaxVal = (realToFrac (quantMaxVal + 1) / 166) * punch config

  when (fromIntegral (BS.length hash) /= 4 + 2 * sizeX * sizeY) (Left InvalidHashLength)

  let firstColor = pixelToLinear $
        PixelRGB8
        (fromIntegral $ Bits.shiftR dcValue 16)
        (fromIntegral $ Bits.shiftR dcValue 8 Bits..&. 255)
        (fromIntegral $ dcValue Bits..&. 255) :: PixelRGBF

  restColor <- foldrM
    (\component acc -> do
        let acValStart = 4 + component * 2
            acValStop = 4 + (component + 1) * 2
            acValRange = acValStop - acValStart
            acValStr = BS.take acValRange . BS.drop acValStart $ hash
        acValue <- base83DecodeTagged acValStr

        let acValue' = realToFrac acValue :: Float
            r = realMaxVal * signPow ((realToFrac @Int (floor (acValue' / (19 * 19))) - 9) / 9) 2
            g = realMaxVal * signPow ((realToFrac @Int (floor (acValue' / 19) `mod` 19) - 9) / 9) 2
            b = realMaxVal * signPow ((realToFrac (acValue  `mod` 19) - 9) / 9) 2
            color = PixelRGBF r g b
        pure (color:acc)
    )
    []
    [1..fromIntegral $ sizeX * sizeY - 1]
  let height = outputHeight config
      width = outputWidth config
      colors = V.fromList (firstColor:restColor)

  pure $ generateImage (decodePixel colors height width sizeY sizeX) width height

-- | Helper function to decode a single pixel.
decodePixel :: V.Vector PixelRGBF -> Int -> Int -> Int -> Int -> Int -> Int -> PixelRGBF
decodePixel colors height width sizeY sizeX x y  = foldl' acc (PixelRGBF 0 0 0) ji
  where
    x' = realToFrac x
    y' = realToFrac y
    height' = realToFrac height
    width' = realToFrac width
    ji = (,) <$> [0..sizeY - 1] <*> [0..sizeX - 1]
    acc (PixelRGBF r g b) (j, i) =
      let i' = realToFrac i
          j' = realToFrac j
          basis = cos (pi * x' * i' / width') * cos (pi * y' * j' / height')
          -- Vector index safe in practice, convered by garbage data and property tests
          PixelRGBF r' g' b' = colors V.! (i + j * sizeX) 
      in PixelRGBF (r + r' * basis) (g + g' * basis) (b + b' * basis)  

-- | Helper function to decode a base83 value or return an errorl
base83DecodeTagged :: BS.ByteString -> Either DecodeError Int
base83DecodeTagged = either (Left . InvalidCharacterError) Right . base83Decode
