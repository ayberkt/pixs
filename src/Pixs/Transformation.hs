{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Pixs.Transformation ( blur
                           , reflectVertical
                           , reflectHorizontal
                           , reflect
                           , changeRed
                           , changeGreen
                           , changeBlue
                           , addAlphaChannel
                           , changeBrightness
                           , changeContrast
                           , negateImage
                           , saturation
                           , getPixel
                           , average
                           , (⊕)
                           , (⊗)) where

import           Codec.Picture         (Image (..), Pixel, PixelRGB8 (..),
                                        PixelRGBA8 (..), generateImage,
                                        imageHeight, imageWidth, pixelAt,
                                        pixelMap)
import           Data.Maybe            (catMaybes)
import           Data.Word
import           Pixs.Operations.Pixel (limit, (⊕), (⊗))
import           Pixs.Types            (Color (..))

addAlphaChannel ∷ Image PixelRGB8 → Image PixelRGBA8
addAlphaChannel = pixelMap addAlphaChannel'
    where addAlphaChannel' (PixelRGB8 r g b) = PixelRGBA8 r g b 0xFF

changeBrightness ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeBrightness amount = pixelMap changeBrightness'
  where changeBrightness' (PixelRGBA8 r g b a) = PixelRGBA8 r' g' b' a
          where f = (⊕ amount)
                [r', g', b'] = f <$> [r, g, b]

changeRed ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeRed = changeColor Red

changeGreen ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeGreen = changeColor Green

changeBlue ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeBlue = changeColor Blue

changeColor ∷ Color → Int → Image PixelRGBA8 → Image PixelRGBA8
changeColor c amount = pixelMap changeColor'
  where changeColor' (PixelRGBA8 r g b a) =
          case c of
            Red   → let r' = r ⊗ amount in PixelRGBA8 r' g  b  a
            Green → let g' = g ⊗ amount in PixelRGBA8 r  g' b  a
            Blue  → let b' = b ⊗ amount in PixelRGBA8 r  g  b' a

saturation ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
saturation amount = changeRed amount . changeGreen amount . changeBlue amount

reflectVertical ∷ Pixel a ⇒ Image a → Image a
reflectVertical img =  generateImage complement w h
  where complement x y = pixelAt img x (imageHeight img - y - 1)
        [w, h]         = [imageWidth, imageHeight] <*> pure img

reflectHorizontal ∷ Pixel a ⇒ Image a → Image a
reflectHorizontal img = generateImage complement w h
  where complement x y = pixelAt img (imageWidth img - x - 1) y
        [w, h]         = [imageWidth, imageHeight] <*> pure img

reflect ∷ Pixel a ⇒ Image a → Image a
reflect = reflectVertical . reflectHorizontal

-- | Take a list of pixels. Return the pixel that is the average color of
-- those pixels.
average ∷ [PixelRGBA8] → PixelRGBA8
average pixs = let avg xs   = sum xs `div` length xs
                   redAvg ∷ Word8
                   redAvg   = fromIntegral $ avg [fromIntegral r ∷ Int
                                                 | (PixelRGBA8 r _ _ _) ← pixs ]
                   greenAvg ∷ Word8
                   greenAvg = fromIntegral $ avg [fromIntegral g ∷ Int
                                                 | (PixelRGBA8 _ g _ _) ← pixs]
                   blueAvg ∷ Word8
                   blueAvg = fromIntegral $ avg [fromIntegral b ∷ Int
                                                | (PixelRGBA8 _ _ b _) ← pixs]
               in PixelRGBA8 redAvg greenAvg blueAvg 255

-- | Try to access pixel at x y. Yield nothing if the given coordinates
-- are out of bounds.
getPixel ∷ Image PixelRGBA8 → Int → Int → Maybe PixelRGBA8
getPixel img x y = let xInBounds = x < imageWidth  img && x >= 0
                       yInBounds = y < imageHeight img && y >= 0
                   in if xInBounds && yInBounds
                      then Just $ pixelAt img x y
                      else Nothing

-- TODO: This is a naive implementation---optimize this.
-- | For every pixel p in an image, assign the average of p's
-- neighborhood at distance n, to p.
blur ∷  Image PixelRGBA8 → Int → Image PixelRGBA8
blur img n = let neighbors x y = catMaybes [getPixel img (x - i) (y - j)
                                           | i ← [(-n)..n]
                                           , j ← [(-n)..n]]
                 w = imageWidth  img
                 h = imageHeight img
             in generateImage (\x y → average (neighbors x y)) w h

-- | Contrast change as described in
-- <http://goo.gl/BTI5Ka here>.
--
-- <<docs/example.png>> <<docs/example-contrast.png>>
changeContrast ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeContrast n img = pixelMap changePixel img
  where
    factor = (259 * (n + 255)) `div` (255 * (259 - n))
    f x = fromIntegral $ limit $ factor * (fromIntegral x - 128) + 128
    changePixel (PixelRGBA8 r g b a) = PixelRGBA8 (f r) (f g) (f b) a

-- | Negate the color of a given image.
negateImage ∷ Image PixelRGBA8 → Image PixelRGBA8
negateImage img = pixelMap negate img
