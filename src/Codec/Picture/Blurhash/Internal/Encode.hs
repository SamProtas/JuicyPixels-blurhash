{-# LANGUAGE FlexibleContexts #-}
module Codec.Picture.Blurhash.Internal.Encode where

import qualified Data.Bits as Bits
import qualified Data.List as List
import Data.Foldable (foldl')

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BS
import Codec.Picture
import Codec.Picture.Types

import Codec.Picture.Blurhash.Internal.DList
import Codec.Picture.Blurhash.Internal.Base83
import Codec.Picture.Blurhash.Internal.Common


data EncodeConfig = EncodeConfig
  { componentsX :: !Int
  , componentsY :: !Int
  } deriving Show

encodeConfigDefault :: EncodeConfig
encodeConfigDefault = EncodeConfig 4 4

data EncodeError
  = InvalidComponents
  | B83EncodingError Int Int
  deriving Show

checkComponent :: Int -> Either EncodeError Int
checkComponent c
  | c < 1 || c > 9 = Left InvalidComponents
  | otherwise = pure c


encodeDynamic :: DynamicImage -> Either EncodeError BS.ByteString
encodeDynamic = encodeDynamicWithConfig encodeConfigDefault

encodeDynamicWithConfig :: EncodeConfig -> DynamicImage -> Either EncodeError BS.ByteString
encodeDynamicWithConfig config = encodeRGB8WithConfig config . convertRGB8  

encodeRGB8 :: Image PixelRGB8 -> Either EncodeError BS.ByteString
encodeRGB8 = encodeRGB8WithConfig encodeConfigDefault

encodeRGB8WithConfig :: EncodeConfig -> Image PixelRGB8 -> Either EncodeError BS.ByteString
encodeRGB8WithConfig config = encodeLinearWithConfig config . sRGBImageToLinear

encodeLinear :: Image PixelRGBF -> Either EncodeError BS.ByteString
encodeLinear = encodeLinearWithConfig encodeConfigDefault

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

base83EncodeTagged :: Int -> Int -> Either EncodeError BS.Builder
base83EncodeTagged toEncode len =
  maybe (Left $ B83EncodingError toEncode len) Right $ base83Encode toEncode len


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
        

data EncodedComponents = EncodedComponents
  !(DList PixelRGBF)
  !Float

encodeDcValue :: PixelRGBF -> Int
encodeDcValue (PixelRGBF r g b) =
  Bits.shiftL (linearToSRGB r) 16 + Bits.shiftL (linearToSRGB g) 8 + linearToSRGB b


encodeAcValue :: Float -> PixelRGBF -> Int
encodeAcValue acCompNormFactor (PixelRGBF r g b) = encodedR + encodedG + encodedB
  where
    encodeColor c = max 0 $ min 18 $ floor $ (signPow (c / acCompNormFactor) 0.5) * 9 + 9.5
    encodedR = encodeColor r * 19 * 19
    encodedG = encodeColor g * 19
    encodedB = encodeColor b


