{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Operations.Pixel where

import           Codec.Picture         (PixelRGBA8(..))
import           Data.Word
import           Control.Arrow         ((***))

limit ∷ (Num c, Ord c) ⇒ c → c
limit = max 0 . min 255

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

pixelDiv ∷ PixelRGBA8 → PixelRGBA8 → PixelRGBA8
pixelDiv = applyOp div

-- | Flow-checked addition operation which we denote with ⊕.
-- Also has an ASCII alias @safeAdd@.
(⊕) ∷ Word8 → Int → Word8
(⊕) x y = let x' = fromIntegral x ∷ Int
          in fromIntegral . limit $ x' + y

-- | ASCII alias for flow-checked addition.
safeAdd ∷ Word8 → Int → Word8
safeAdd = (⊕)

-- | Flow-checked multiplication operation.
-- Also has an ASCII alias `safeMultiply`.
(⊗) ∷ Word8 → Int → Word8
(⊗) x y = let x' = (fromIntegral x) ∷ Int
          in fromIntegral . limit $ x' * y

-- | ASCII alias for flow-checked multiplication.
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
