{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Filter where

import           Codec.Picture       (Image (..), PixelRGBA8 (..),
                                      generateImage, pixelAt, pixelMap)
import           Data.Word           (Word8)
import           Pixs.Operations.Pixel (average)

blackAndWhite ∷ Image PixelRGBA8 → Image PixelRGBA8
blackAndWhite = let decolor (PixelRGBA8 r g b a)
                      = let r'   = fromIntegral r ∷ Int
                            g'   = fromIntegral g ∷ Int
                            b'   = fromIntegral b ∷ Int
                            avg  = (r' + g' + b') `div` 3
                            avg' = fromIntegral avg ∷ Word8
                        in PixelRGBA8 avg' avg' avg' a
                in pixelMap decolor

pixelate ∷ Image PixelRGBA8 → Image PixelRGBA8
pixelate img = let blockAvg x y = average $ pixelAt img <$> map (+ x) [0,1]
                                                        <*> map (+ y) [0,1]
               in generateImage blockAvg (imageWidth img - 1) (imageHeight img - 1)
