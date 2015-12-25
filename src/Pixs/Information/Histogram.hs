{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Information.Histogram where

import           Codec.Picture ( Image
                               , PixelRGBA8(..)
                               , imageHeight
                               , imageWidth
                               , pixelAt)
import           Data.Map      ( Map
                               , adjust
                               , fromList)
import           Data.Word     (Word8)
import           Prelude       hiding (lookup)

data Color = Red
           | Green
           | Blue
           deriving (Show,Eq,Ord)

colorCount ∷ Image PixelRGBA8 → Color → Map Word8 Int
colorCount img c = let count m x y
                         | y == imageHeight img - 1 = m
                         | x == imageWidth  img - 1 = count m' 0 (succ y)
                         | otherwise = count m' (succ x) y
                         where (PixelRGBA8 r g b _) = pixelAt img x y
                               m' = case c of Red   → adjust (+ 1) r m
                                              Green → adjust (+ 1) g m
                                              Blue  → adjust (+ 1) b m
                       initMap = fromList $ map (\x → (x,0)) [0..255]
                   in count initMap 0 0

redCount ∷ Image PixelRGBA8 → Map Word8 Int
redCount img = colorCount img Red

