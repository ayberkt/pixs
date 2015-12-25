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

redCount ∷ Image PixelRGBA8 → Map Word8 Int
redCount img = let count ∷ Map Word8 Int → Int → Int → Map Word8 Int
                   count m x y
                     | y == imageHeight img - 1 = m
                     | x == imageWidth  img - 1 = count m' 0 (succ y)
                     | otherwise = count m' (succ x) y
                     where r' = case pixelAt img x y of (PixelRGBA8 r _ _ _) → r
                           m' = adjust (+ 1) r' m
                   initMap = fromList $ map (\x → (x, 0)) [0..255]
               in count initMap 0 0

blueCount ∷ Image PixelRGBA8 → Map Word8 Int
blueCount img = let count ∷ Map Word8 Int → Int → Int → Map Word8 Int
                    count m x y
                      | y == imageHeight img - 1 = m
                      | x == imageWidth  img - 1 = count m' 0 (succ y)
                      | otherwise = count m' (succ x) y
                      where r' = case pixelAt img x y of (PixelRGBA8 _ _ b _) → b
                            m' = adjust (+ 1) r' m
                    initMap = fromList $ map (\x → (x, 0)) [0..255]
                in count initMap 0 0
