{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Filter where

import Codec.Picture ( PixelRGBA8(..)
                     , Image(..)
                     , pixelMap)
import Data.Word (Word8(..))

blackAndWhite ∷ Image PixelRGBA8 → Image PixelRGBA8
blackAndWhite img = let decolor (PixelRGBA8 r g b a)
                          = let r' = fromIntegral r ∷ Int
                                g' = fromIntegral g ∷ Int
                                b' = fromIntegral b ∷ Int
                                avg = (r' + g' + b') `div` 3
                                avg' = fromIntegral avg ∷ Word8
                            in PixelRGBA8 avg' avg' avg' a
                    in pixelMap decolor img
