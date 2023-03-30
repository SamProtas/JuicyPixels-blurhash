{-# LANGUAGE CPP #-}
-- |
-- Module: Codec.Picture.Blurhash
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Blurhash is a very compact representation of a placeholder for an image.
--
-- This library provides a Blurhash encoding and decoding implementation based on the JuicyPixels representation of images.
--
-- For the full Blurhash sales pitch and algorithm explanation see either of:
-- 
--   - The website: <https://blurha.sh/>
-- 
--   - The central git repo: <https://github.com/woltapp/blurhash>
--
-- An image such as:
-- 
-- <<docs/example.jpg>>
--
-- Can be encoded as:
--
-- LGFFaWYk^6#M@-5c,1Ex\@\@or[j6o
--
-- Which your client can render as:
--
-- <<docs/blurred.png>>
--
module Codec.Picture.Blurhash
  (
    -- * Example Use
    -- $setup
    -- $exampleEncode
    -- $exampleDecode
    
    -- * Encoding Images
    -- $encoding

    -- ** Default
    -- $defencoding
    encodeDynamic
  , encodeRGB8
  , encodeLinear

    -- ** Custom
  , encodeDynamicWithConfig
  , encodeRGB8WithConfig
  , encodeLinearWithConfig

    -- ** Configuration
  , encodeConfigDefault
  , EncodeConfig
  , componentsX
  , componentsY

    -- ** Errors
  , EncodeError(..)

    -- * Decoding To Images
    -- $decoding

    -- ** Default
    -- $defdecoding
  , decodeRGB8
  , decodeLinear

    -- ** Custom
  , decodeRGB8WithConfig
  , decodeLinearWithConfig

    -- ** Configuration
  , decodeConfigDefault
  , DecodeConfig
  , punch
  , outputWidth
  , outputHeight

    -- ** Errors
  , DecodeError(..)
  ) where

import System.FilePath (FilePath, (</>), takeDirectory)
-- Imported for Haddock links
import Codec.Picture (DynamicImage, Image, PixelRGB8, PixelRGBF)

import Codec.Picture.Blurhash.Internal.Encode
import Codec.Picture.Blurhash.Internal.Decode


-- $setup
-- First some imports.
--
-- >>> :set -XOverloadedStrings
-- >>> import           Codec.Picture (readImage)
-- >>> import qualified Codec.Picture.Blurhash as BH
-- >>> import           Data.ByteString.Lazy (ByteString)
--

-- $exampleEncode
-- Given a an image filepath @(imgFilePath :: FilePath)@.
--
-- >>> :{
--  do
--    Right img <- readImage imgFilePath
--    print $ BH.encodeDynamic img
-- :}
-- Right "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na"
--
-- Now you can store this nice compact encoding in a database column and send it to the client along with the asset path to the full size image.
--

-- $exampleDecode
-- If you're lucky enough to write your client in Haskell, you receive that encoding and draw a Blurhash placeholder while the full asset is fetched.
--
-- >>> let Right myBlurryPlaceholderImg = BH.decodeRGB8 "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na"
--
  
-- $encoding
--
-- The Blurhash algorithm natively supports encoding 'Image' 'PixelRGB8' and 'Image' 'PixelRGBF'. This library additionally supports encoding a 'DynamicImage' via conversion to 'Image' 'PixelRGB8'.

-- $defencoding
--
-- Encode various image representations to a blurhash using 'encodeConfigDefault'.

-- $decoding
--
-- The Blurhash algorithm natively supports decoding into 'Image' 'PixelRGB8' and 'Image' 'PixelRGBF'.

-- $defdecoding
--
-- Decode a blurhash to various image representations using 'decodeConfigDefault'.


imgDir :: FilePath
imgDir = takeDirectory __FILE__ </> ".." </> ".." </> ".." </> "imgs"

imgFilePath :: FilePath
imgFilePath = imgDir </> "cool_cat.jpg"
