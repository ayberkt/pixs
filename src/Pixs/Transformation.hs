{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UnicodeSyntax      #-}

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
                           , safeAdd
                           , (⊗)
                           , safeMultiply
                           , pixelDiv
                           , scale) where


import Control.Arrow ((***))
import Data.Word
import Data.Maybe (catMaybes)
import Codec.Picture( PixelRGBA8(..)
                    , PixelRGB8(..)
                    , Image(..)
                    , Pixel
                    , pixelMap
                    , imageHeight
                    , imageWidth
                    , pixelAt
                    , generateImage)
import Pixs.Types (Color(..))

limit ∷ (Num c, Ord c) ⇒ c → c
limit = max 0 . min 255

-- | Used for reducing repetition when declaring Num instance.
--   Our strategy for overflow/underflow checking is the same for all of the
--   operations so we define this function that takes in an operation and two
--   pixels and applies the operation to the components. Pixel addition for
--   example is implemented by simply passing (+) to `applyOp`.
applyOp ∷ (Int → Int → Int) → PixelRGBA8 → PixelRGBA8 → PixelRGBA8
applyOp op (PixelRGBA8 r₁ g₁ b₁ a₁) (PixelRGBA8 r₂ g₂ b₂ a₂)
  = PixelRGBA8 r g b (max a₁ a₂)
  where
    f = fromIntegral . limit . uncurry op . (fromIntegral *** fromIntegral)
    r = f (r₁, r₂)
    g = f (g₁, g₂)
    b = f (b₁, b₂)

--   TODO: PixelRGBA8 should not really have an instance of
--   Num since it doesn't behave like a number. For
--   now we declare a num instance for the convenience of
--   being able to use (+), (-) etc... It would be the best
--   it were a VectorSpace if a VectorSpace typeclass exists
--   somewhere. Or maybe, that's too much; I don't know.
instance Num PixelRGBA8 where

  negate (PixelRGBA8 r g b _) = PixelRGBA8 r' g' b' 255
    where (r', g', b') = (255 - r, 255 - g, 255 - b)

  (+) = applyOp (+)

  (-) = applyOp (-)

  (*) = applyOp (*)

  abs p = p

  signum p = p

  fromInteger _ = undefined


pixelDiv ∷ PixelRGBA8 → PixelRGBA8 → PixelRGBA8
pixelDiv = applyOp div

-- | Flow-checked addition operation which we denote with ⊕.
-- Also has an ASCII alias `safeAdd`.
(⊕) ∷ Word8 → Int → Word8
(⊕) x y = let x' = fromIntegral x ∷ Int
          in fromIntegral . limit $ x' + y

safeAdd ∷ Word8 → Int → Word8
safeAdd = (⊕)

-- | Flow-checked multiplication operation.
-- Also has an ASCII alias `safeMultiply`.
(⊗) ∷ Word8 → Int → Word8
(⊗) x y = let x' = (fromIntegral x) ∷ Int
          in fromIntegral . limit $ x' * y

safeMultiply ∷ Word8 → Int → Word8
safeMultiply = (⊗)

-- | Scalar multiplication.
scale ∷ Double → PixelRGBA8 → PixelRGBA8
scale n (PixelRGBA8 r g b a) = PixelRGBA8 r' g' b' a
  where
    f = round . limit . (* n) . fromIntegral
    r' = f r
    g' = f g
    b' = f b

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
-- <http://www.dfstudios.co.uk/articles/programming/image-programming-algorithms/image-processing-algorithms-part-5-contrast-adjustment/ here>.
changeContrast ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeContrast n img = pixelMap changePixel img
  where
    factor = (259 * (n + 255)) `div` (255 * (259 - n))
    f x = fromIntegral $ limit $ factor * (fromIntegral x - 128) + 128
    changePixel (PixelRGBA8 r g b a) = PixelRGBA8 (f r) (f g) (f b) a

-- | Negate the color of a given image.
negateImage ∷ Image PixelRGBA8 → Image PixelRGBA8
negateImage img = pixelMap negate img
