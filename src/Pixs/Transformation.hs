{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Pixs.Transformation where


import Data.Word
import Codec.Picture( PixelRGBA8(..)
                    , Image(..)
                    , Pixel
                    , pixelMap
                    , imageHeight
                    , imageWidth
                    , pixelAt
                    , generateImage)

-- | Used for reducing repetition when declaring Num instance.
--   Our strategy for overflow/underflow checking is the same for all of the
--   operations so we define this function that takes in an operation and two
--   pixels and applies the operation to the components. Pixel addition for
--   example is implemented by simply passing (+) to `applyOp`.
applyOp ∷ (Int → Int → Int) → PixelRGBA8 → PixelRGBA8 → PixelRGBA8
applyOp op (PixelRGBA8 r₁ g₁ b₁ a₁) (PixelRGBA8 r₂ g₂ b₂ a₂)
  = PixelRGBA8 r g b (max a₁ a₂)
  where r' = (fromIntegral r₁ `op` fromIntegral r₂)
        g' = (fromIntegral g₁ `op` fromIntegral g₂)
        b' = (fromIntegral b₁ `op` fromIntegral b₂)
        r  = fromIntegral . max 0 . min 255 $ r'   ∷ Word8
        g  = fromIntegral . max 0 . min 255 $ g'   ∷ Word8
        b  = fromIntegral . max 0 . min 255 $ b'   ∷ Word8

-- | TODO: PixelRGBA8 should not really have an instance of
--   Num since it doesn't really behave like a number. For
--   now we declare a num instance for the convenience of
--   being able to use (+), (-) etc... It would be the best
--   it were a VectorSpace if a VectorSpace typeclass exists
--   somewhere. Or maybe, that's too much; I don't know.
instance Num PixelRGBA8 where

  negate (PixelRGBA8 r g b _) = PixelRGBA8 r' g' b' 255
    where r' = 255 - r
          g' = 255 - g
          b' = 255 - b

  (+) = applyOp (+)

  (-) = applyOp (-)

  (*) = applyOp (*)

  abs p = p

  signum p = p

  fromInteger _ = undefined

-- | Flow-checked addition operation which we denote with ⊕.
(⊕) ∷ Word8 → Int → Word8
(⊕) x y = let x' = (fromIntegral x) ∷ Int
              y' = (fromIntegral y) ∷ Int
          in fromIntegral . max 0 . min 255 $ x' + y'

-- | Flow-checked multiplication operation.
(⊗) ∷ Word8 → Int → Word8
(⊗) x y = let x' = (fromIntegral x) ∷ Int
              y' = (fromIntegral y) ∷ Int
          in fromIntegral . max 0 . min 255 $ x' * y'


-- | Scalar multiplication.
scale ∷ Int → PixelRGBA8 → PixelRGBA8
scale n (PixelRGBA8 r g b a) = PixelRGBA8 (r ⊗ n) (g ⊗ n) (b ⊗ n) a

changeBrightness ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeBrightness amount = pixelMap changeBrightness'
  where changeBrightness' (PixelRGBA8 r g b a) = PixelRGBA8 r' g' b' a
          where r' = r ⊕ amount
                g' = g ⊕ amount
                b' = b ⊕ amount

changeRed ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeRed amount = pixelMap changeRed'
  where changeRed' (PixelRGBA8 r g b a) = PixelRGBA8 (r ⊕ amount) g b a

changeGreen ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeGreen amount = pixelMap changeGreen'
  where changeGreen' (PixelRGBA8 r g b a) = PixelRGBA8 r g' b a
                                            where g' = g ⊕ amount

changeBlue ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
changeBlue amount = pixelMap changeBlue'
  where changeBlue' (PixelRGBA8 r g b a) = PixelRGBA8 r g b' a
                                            where b' = b ⊕ amount

saturation ∷ Int → Image PixelRGBA8 → Image PixelRGBA8
saturation amount = changeRed amount . changeGreen amount . changeBlue amount

flipVertical ∷ Pixel a ⇒ Image a → Image a
flipVertical img =  generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img x (imageHeight img - y - 1)

flipHorizontal ∷ Pixel a ⇒ Image a → Image a
flipHorizontal img = generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img (imageWidth img - x - 1) y

flip ∷ Pixel a ⇒ Image a → Image a
flip = flipVertical . flipHorizontal

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

-- TODO: There are problems with this; fix them.
blur ∷  Image PixelRGBA8 → Int → Image PixelRGBA8
blur img n = let neighbors x y
                   | x <= n || y <= n             = [pixelAt img x y]
                   | xOutOfBounds && yOutOfBounds = [ pixelAt img x y
                                                    , pixelAt img (x - 1) y
                                                    , pixelAt img x (y - 1)
                                                    , pixelAt img (x - 1)
                                                                  (y - 1)]
                   | xOutOfBounds = [pixelAt img x y]
                   | yOutOfBounds = [pixelAt img x y]
                   | otherwise    = [pixelAt img (x - i) (y - j)
                                    | i ← [(-n)..n], j ← [(-n)..n]]
                   where xOutOfBounds = x >= imageWidth  img - n
                         yOutOfBounds = y >= imageHeight img - n
                 blurPixel x y = average (neighbors x y)
             in generateImage blurPixel (imageWidth img) (imageHeight img)

-- | Negate every pixel.
negateImage ∷ Image PixelRGBA8 → Image PixelRGBA8
negateImage img = pixelMap negate img
