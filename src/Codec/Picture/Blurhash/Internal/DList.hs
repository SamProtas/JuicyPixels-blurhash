-- |
-- Module: Codec.Picture.Blurhash.Internal.DList
-- Copyright: (c) 2020 Sam Protas
-- License: BSD3
-- Maintainer: Sam Protas <sam.protas@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Internal difference list implementation.
--
-- __Note__: This is an internal module not subject to PVP adherence.
module Codec.Picture.Blurhash.Internal.DList where

-- | A type alias for a Difference list for efficient appents
type DList a = [a] -> [a]

-- | Convert a list to a difference list
toDList :: [a] -> DList a
toDList l = \l' -> l ++ l'

-- | Convert a difference list to a list
dListToList :: DList a -> [a]
dListToList = ($[])

-- | Append an item to the end of a difference list
dListSnoc :: DList a -> a -> DList a
dListSnoc dlist a = dlist . (a:)
