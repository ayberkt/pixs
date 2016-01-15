{-# LANGUAGE UnicodeSyntax #-}

module Pixs.PointOperations where

import Pixs.Information.Histogram            (colorCount)
import            Codec.Picture              (Image, PixelRGBA8(..),
                                              pixelAt, generateImage,
                                             imageWidth, imageHeight)
import            Pixs.Types                 (Color(..))
import qualified  Data.Map                   as M

threshImage ∷ Int
          → Image PixelRGBA8
          → Image PixelRGBA8
threshImage threshold img =
  let black                       = PixelRGBA8 0    0    0    0xFF
      white                       = PixelRGBA8 0xFF 0xFF 0xFF 0XFF
      [redMap, greenMap, blueMap] = colorCount img <$> [Red, Green, Blue]
      -- Dictionary meaning of "to thresh": separate grain from (a plant),
      -- typically with a flail or by the action of a revolving mechanism:
      -- machinery that can reap and thresh corn in the same process | (as noun
      -- threshing) : farm workers started the afternoon's threshing.
      thresh x y                  = let (PixelRGBA8 r g b _) = pixelAt img x y
                                        fs ■ xs = map (uncurry ($)) $ zip fs xs
                                        intensity = sum
                                                    $ (M.findWithDefault 0 <$> [r, g, b])
                                                    ■ [redMap, greenMap, blueMap]
                      in if (intensity > threshold) then white else black
  in generateImage thresh (imageWidth img) (imageHeight img)
