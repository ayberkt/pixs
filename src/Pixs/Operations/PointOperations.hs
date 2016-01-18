{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Operations.PointOperations where

import Pixs.Information.Histogram            (colorCount)
import            Codec.Picture              (Image, PixelRGBA8(..),
                                              pixelAt, generateImage,
                                             imageWidth, imageHeight)
import            Pixs.Types                 (Color(..))
import qualified  Data.Map                   as M

-- | Thresholds an image by pixel intensity. We define the intensity of a given
-- pixel (PixelRGBA8 r g b a)¹ to be the sum of the number of pixels with the
-- components r, g, b, and a. @threshold@ sifts out all pixels that have
-- intensity greater than the given threshold `n`. This is useful for things
-- like separating the backfground from the foreground. A complete explanation
-- of thresholding is given at:
-- http://homepages.inf.ed.ac.uk/rbf/HIPR2/threshld.htm
--
-- <<docs/example.png>> <<docs/example-thresholded.png>>
--
-- ¹: If that's not clear, I'm doing destructuring in the English language.
threshold ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
threshold n img =
  let black                       = PixelRGBA8 0    0    0    0xFF
      white                       = PixelRGBA8 0xFF 0xFF 0xFF 0XFF
      [redMap, greenMap, blueMap] = colorCount img <$> [Red, Green, Blue]
      -- Dictionary meaning of "to thresh": separate grain from (a plant),
      -- typically with a flail or by the action of a revolving mechanism:
      -- machinery that can reap and thresh corn in the same process | (as noun
      -- threshing) : farm workers started the afternoon's threshing.
      thresh x y = let (PixelRGBA8 r g b _) = pixelAt img x y
                       (■) = zipWith id
                       intensity = (sum $ (M.findWithDefault 0 <$> [r, g, b])
                                    ■ [redMap, greenMap, blueMap]) `div` 3
                      in if (intensity > n) then black else white
  in generateImage thresh (imageWidth img) (imageHeight img)
