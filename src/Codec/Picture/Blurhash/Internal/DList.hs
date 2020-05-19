module Codec.Picture.Blurhash.Internal.DList where


type DList a = [a] -> [a]

toDList :: [a] -> DList a
toDList l = \l' -> l ++ l'

dListToList :: DList a -> [a]
dListToList = ($[])

dListSnoc :: DList a -> a -> DList a
dListSnoc dlist a = dlist . (a:)
