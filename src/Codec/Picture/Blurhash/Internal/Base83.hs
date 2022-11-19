{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Codec.Picture.Blurhash.Internal.Base83
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal Base83 encoding/decoding implementation.
--
-- __Note__: This is an internal module not subject to PVP adherence.
module Codec.Picture.Blurhash.Internal.Base83 where

import Control.Monad (guard)
import Data.Foldable (foldlM)
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import qualified Data.Vector as V


-- | Base 83 encoding per the 'alphabet' specified by Blurhash
base83Encode :: Int -> Int -> Maybe BS.Builder
base83Encode toEncode len = do
  guard $ (toEncode `div` 83 ^ len) == 0

  mconcat <$>
    traverse
    (\i -> do
        let digit = toEncode `div` (83 ^ (len - i)) `mod` 83
        BS.word8 <$> alphabet V.!? digit)
    [1..len]

-- | Base 83 decoding per the 'alphabet' specified by Blurhash
base83Decode :: BS.ByteString -> Either Word8 Int
base83Decode toDecode = foldlM acc 0 $ BS.unpack toDecode
  where
    acc ret word = maybe (Left word) (Right . (+ (ret * 83))) $  Map.lookup word charToIndex


-- | Helper function providing a mapping from a byte to it's index in the Blurhash 'alphabet'
charToIndex :: Map.Map Word8 Int
charToIndex = V.ifoldl' (\mapping index char -> Map.insert char index mapping) mempty alphabet

-- | The Blurhash 'alphabet'
alphabet :: V.Vector Word8
alphabet = V.fromList $
           BS.unpack
           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
