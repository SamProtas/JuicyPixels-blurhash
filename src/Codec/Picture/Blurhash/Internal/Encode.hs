{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Codec.Picture.Blurhash.Internal.Encode
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal Blurhash encoding implementation.
--
-- __Note__: This is an internal module not subject to PVP adherence.
module Codec.Picture.Blurhash.Internal.Encode where

import qualified Data.Bits as Bits
import qualified Data.List as List
import Data.Foldable (foldl')
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import Codec.Picture (DynamicImage, Image(..), PixelRGB8(..), PixelRGBF(..), convertRGB8, colorMap)
import Codec.Picture.Types (pixelFold, ColorConvertible(..)) -- ColorConvertible imported for haddocks

import Codec.Picture.Blurhash.Internal.DList
import Codec.Picture.Blurhash.Internal.Base83
import Codec.Picture.Blurhash.Internal.Common

-- | Configuration for how to encode an image into a blurhash.
--
-- Create custom configs using record update syntax and 'encodeConfigDefault'.
--
-- >>> let myEncodeConfig = encodeConfigDefault { componentsX = 4, componentsY = 3 }
--
data EncodeConfig = EncodeConfig
  { componentsX :: !Int
  -- ^ Number of components along the X axis. 
  --
  -- See 'EncodeConfig' for example use with record update syntax.
  , componentsY :: !Int
  -- ^ Number of components along the Y axis.
  --
  -- See 'EncodeConfig' for example use with record update syntax.
  } deriving (Show, Generic)

-- | Encoding error types.
data EncodeError
  = InvalidComponents -- ^ The provided config components were invalid.
  | B83EncodingError Int Int -- ^ The provided number cannot be base83 encoded into the provided length.
  deriving (Show, Generic)

-- | A reasonable default configuration for encoding.
--
-- >>> componentsX encodeConfigDefault == 4
-- True
--
-- >>> componentsY encodeConfigDefault == 4
-- True
encodeConfigDefault :: EncodeConfig
encodeConfigDefault = EncodeConfig 4 4

-- | A helper funciton to validate the provided encoding component count.
checkComponent :: Int -> Either EncodeError Int
checkComponent c
  | c < 1 || c > 9 = Left InvalidComponents
  | otherwise = pure c

-- | Encode a 'DynamicImage' to a blurhash. Calls 'encodeDynamicWithConfig' with 'encodeConfigDefault'.
--
-- Note: Relies on 'convertRGB8' before proceding with the standard Blurhash algorithm.
encodeDynamic :: DynamicImage -> Either EncodeError BS.ByteString
encodeDynamic = encodeDynamicWithConfig encodeConfigDefault

-- | Encode a 'DynamicImage' to a blurhash with a given an 'EncodeConfig'.
--
-- Note: Relies on 'convertRGB8' before proceding with the standard Blurhash algorithm.
encodeDynamicWithConfig :: EncodeConfig -> DynamicImage -> Either EncodeError BS.ByteString
encodeDynamicWithConfig config = encodeRGB8WithConfig config . convertRGB8  

-- | Encode an 'Image' 'PixelRGB8' to a blurhash. Calls 'encodeRGB8WithConfig' with 'encodeConfigDefault'.
--
-- Note: This is the most direct port of other language's implementation's default encoding function.
encodeRGB8 :: Image PixelRGB8 -> Either EncodeError BS.ByteString
encodeRGB8 = encodeRGB8WithConfig encodeConfigDefault

-- | Encode an 'Image' 'PixelRGB8' to a blurhash given an 'EncodeConfig'.
--
-- Note: This is the most direct port of other languages implementation's encoding function.
encodeRGB8WithConfig :: EncodeConfig -> Image PixelRGB8 -> Either EncodeError BS.ByteString
encodeRGB8WithConfig config = encodeLinearWithConfig config . sRGBImageToLinear

-- | Encode an 'Image' 'PixelRGBF' to a blurhash. Calls 'encodeLinearWithConfig' with 'encodeConfigDefault'.
--
-- Note: Blurhash implementations use a non-naive 'PixelRGB8' to 'PixelRGBF' conversion. Beware that using 'promotePixel' or 'promoteImage' from 'ColorConvertible' to convert an 'Image' 'PixelRGB8' to an 'Image' 'PixelRGBF' before using 'encodeLinear' will give different results than 'encodeRGB8'.
encodeLinear :: Image PixelRGBF -> Either EncodeError BS.ByteString
encodeLinear = encodeLinearWithConfig encodeConfigDefault

-- | Encode an 'Image' 'PixelRGBF' to a blurhash given an 'EncodeConfig'.
--
-- Note: Blurhash implementations use a non-naive 'PixelRGB8' to 'PixelRGBF' conversion. Beware that using 'promotePixel' or 'promoteImage' from 'ColorConvertible' to convert an 'Image' 'PixelRGB8' to an 'Image' 'PixelRGBF' before using 'encodeLinearWithConfig' will give different results than 'encodeRGB8WithConfig'.
encodeLinearWithConfig
  :: EncodeConfig
  -> Image PixelRGBF
  -> Either EncodeError BS.ByteString
encodeLinearWithConfig config img = do
  cx <- checkComponent . componentsX $ config
  cy <- checkComponent . componentsY $ config
  
  let EncodedComponents compDList maxAC = encodeComponents cx cy img
      components = dListToList compDList
  (firstComp, restComp) <- maybe (Left InvalidComponents) Right $ List.uncons components
  let dcValue = encodeDcValue firstComp
      quantMaxAcComp = max 0 $ min 82 $ floor $ maxAC * 166 - 0.5
      acCompNormFactor = (fromIntegral . succ $ quantMaxAcComp) / 166
      acValues = fmap (encodeAcValue acCompNormFactor) restComp
  e1 <- base83EncodeTagged ((cx - 1) + ((cy - 1) * 9)) 1
  e2 <- base83EncodeTagged quantMaxAcComp 1
  e3 <- base83EncodeTagged dcValue 4
  e4 <- mconcat <$> traverse (flip base83EncodeTagged 2) acValues
  pure $ BS.toLazyByteString $ e1 <> e2 <> e3 <> e4

-- | Helper function to encode a base83 value or return an error.
base83EncodeTagged
  :: Int -- ^ toEncode
  -> Int -- ^ encoded length
  -> Either EncodeError BS.Builder
base83EncodeTagged toEncode len =
  maybe (Left $ B83EncodingError toEncode len) Right $ base83Encode toEncode len

-- | Encode encode the color components.
encodeComponents :: Int -> Int -> Image PixelRGBF -> EncodedComponents
encodeComponents compX compY img = foldl' acc start ji
  where
    acc (EncodedComponents comps maxAC) (j, i) =
      let isFirst = i == 0 && j == 0
          normFactor = if isFirst then 1 else 2
          component@(PixelRGBF r g b) = encodeComponent i j normFactor img
          maxAC' = if isFirst then maxAC else maximum [maxAC, abs r, abs g, abs b]
      in EncodedComponents (dListSnoc comps  component) maxAC'
    ji = (,) <$> [0..compY - 1] <*> [0..compX - 1]
    start = EncodedComponents (toDList []) 0

-- | Encode a single color component.    
encodeComponent :: Int -> Int -> Float -> Image PixelRGBF -> PixelRGBF
encodeComponent i j normFactor img = colorMap (/hw') $ pixelFold acc (PixelRGBF 0 0 0) img
  where
    acc (PixelRGBF r g b) x y (PixelRGBF r' g' b') =
      let x' = realToFrac x
          y' = realToFrac y
          basis = normFactor * cos (pi * i' * x'  / width') * cos (pi * j' * y' / height')
      in PixelRGBF (r + r' * basis) (g + g' * basis) (b + b' * basis)
    width = imageWidth img
    width' = realToFrac width
    height' = realToFrac . imageHeight $ img
    hw' = width' * height'
    i' = realToFrac i
    j' = realToFrac j
        
-- | Intermediate data for calculating color components and macAC in one pass.
data EncodedComponents = EncodedComponents
  !(DList PixelRGBF)
  !Float

-- | Helper function for encoding the DC value.
encodeDcValue :: PixelRGBF -> Int
encodeDcValue (PixelRGBF r g b) =
  Bits.shiftL (linearToSRGB r) 16 + Bits.shiftL (linearToSRGB g) 8 + linearToSRGB b

-- | Helper function for encoding an AC value.
encodeAcValue :: Float -> PixelRGBF -> Int
encodeAcValue acCompNormFactor (PixelRGBF r g b) = encodedR + encodedG + encodedB
  where
    encodeColor c = max 0 $ min 18 $ floor $ (signPow (c / acCompNormFactor) 0.5) * 9 + 9.5
    encodedR = encodeColor r * 19 * 19
    encodedG = encodeColor g * 19
    encodedB = encodeColor b


