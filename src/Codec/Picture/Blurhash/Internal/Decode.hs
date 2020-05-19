{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Picture.Blurhash.Internal.Decode where

import Control.Monad (when)
import qualified Data.Bits as Bits
import Data.Foldable (foldrM, foldl')
import Data.Word

import Codec.Picture
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Codec.Picture.Blurhash.Internal.Base83
import Codec.Picture.Blurhash.Internal.Common


data DecodeConfig = DecodeConfig
  { punch :: Float
  , outputWidth :: Int
  , outputHeight :: Int
  } deriving Show

data DecodeError
  = InvalidCharacterError Word8
  | InvalidHashLength
  deriving Show


decodeConfigDefault :: DecodeConfig
decodeConfigDefault = DecodeConfig 1 32 32

decodeRGB8 :: BS.ByteString -> Either DecodeError (Image PixelRGB8)
decodeRGB8 = decodeRGB8WithConfig decodeConfigDefault

decodeRGB8WithConfig :: DecodeConfig -> BS.ByteString -> Either DecodeError (Image PixelRGB8)
decodeRGB8WithConfig config blurhash
  = linearImageToSRGB <$> decodeLinearWithConfig config blurhash

decodeLinear :: BS.ByteString -> Either DecodeError (Image PixelRGBF)
decodeLinear = decodeLinearWithConfig decodeConfigDefault

decodeLinearWithConfig
  :: DecodeConfig
  -> BS.ByteString
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
          PixelRGBF r' g' b' = colors V.! (i + j * sizeX) -- TODO - unsafe?
      in PixelRGBF (r + r' * basis) (g + g' * basis) (b + b' * basis)  


base83DecodeTagged :: BS.ByteString -> Either DecodeError Int
base83DecodeTagged = either (Left . InvalidCharacterError) Right . base83Decode
