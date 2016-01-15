{-# LANGUAGE UnicodeSyntax #-}

module Pixs.PointOperations where

import Pixs.Information.Histogram            (colorCount)
import            Codec.Picture              (Image, PixelRGBA8(..),
                                              pixelAt, generateImage,
                                             imageWidth, imageHeight)
import            Pixs.Types                 (Color(..))
import qualified  Data.Map                   as M
import            Data.Word                  ()

threshold ∷ Color
          → Int
          → Image PixelRGBA8
          → Image PixelRGBA8
threshold c trsh img =
  let black         = PixelRGBA8 0    0    0    0xFF
      white         = PixelRGBA8 0xFF 0xFF 0xFF 0XFF
      colorMap      = colorCount img c
      foo x y       = let (PixelRGBA8 r g b _) = pixelAt img x y
                          value = case c of Red   → r
                                            Green → g
                                            Blue  → b
                      in if (M.findWithDefault 0 value colorMap > trsh)
                         then white
                         else black
  in generateImage foo (imageWidth img) (imageHeight img)
