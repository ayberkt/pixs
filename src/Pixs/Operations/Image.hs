{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Operations.Image where

import           Codec.Picture          ( PixelRGBA8(..)
                                        , Pixel8
                                        , Image(..)
                                        -- , pixelMap
                                        , imageHeight
                                        , imageWidth
                                        , pixelAt
                                        , generateImage)

import           Pixs.Transformation    ()
import           Pixs.Transformation    (pixelDiv, scale)
import           Data.Bits              ((.&.), (.|.))
import           Prelude   hiding       (sum)

-- | Takes as input two identically sized images @img₁@ and @img₂@, and produces
-- a new image by summing each pixel of @img₁@ with the corresponding pixel
-- from @img₂@.
add ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
add img₁ img₂ = let sum x y = (pixelAt img₁ x y) + (pixelAt img₂ x y)
                in generateImage sum (imageWidth img₁) (imageHeight img₁)

-- | Takes as input two identically sized images @img₁@ and @img₂@, and
-- produces a new image by subtracting the pixel at @img₂@ from the
-- corresponding one at @img₂@.
subtract ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
subtract img₁ img₂ = let sub x y = (pixelAt img₁ x y) - (pixelAt img₂ x y)
                     in generateImage sub (imageWidth img₁) (imageHeight img₂)

-- | Takes as input two identically sized images, @img₁@ and @img₂@. Creates
-- a new image by multiplying the corresponding two pixels from the two.
multiply ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
multiply img₁ img₂ = let mul x y = (pixelAt img₁ x y) - (pixelAt img₂ x y)
                     in generateImage mul (imageWidth img₁) (imageHeight img₂)

divide ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
divide img₁ img₂ = let x ÷ y = (pixelAt img₁ x y) `pixelDiv` (pixelAt img₂ x y)
                   in generateImage (÷) (imageWidth img₁) (imageHeight img₂)

blend ∷ Image PixelRGBA8 → Image PixelRGBA8 → Double → Image PixelRGBA8
blend img₁ img₂ n = let x % y = n `scale` (pixelAt img₁ x y) + (pixelAt img₂ x y)
                    in generateImage (%) (imageWidth img₁) (imageHeight img₂)

bitwiseImageOp ∷ (Pixel8 → Pixel8 → Pixel8)
               → Image PixelRGBA8
               → Image PixelRGBA8
               → Image PixelRGBA8
bitwiseImageOp op img₁ img₂ =
  let pixelAnd (PixelRGBA8 r₁ g₁ b₁ a₁) (PixelRGBA8 r₂ g₂ b₂ a₂) =
        PixelRGBA8 (r₁ `op` r₂) (g₁ `op` g₂) (b₁ `op` b₂) (max a₁ a₂)
      pixelAnd' x y = pixelAnd (pixelAt img₁ x y) (pixelAt img₂ x y)
  in generateImage pixelAnd' (imageWidth img₁) (imageHeight img₂)

-- | Create a new image by and'ing (i.e., @(.&.)@ from @Data.Bits@) each color
-- component of every two corresponding pixel from @img₁@ and @img₂@.
and ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
and = bitwiseImageOp (.&.)

-- | Create a new image by or'ing (i.e., @(.|.)@ from @Data.Bits@) each color
-- component of every two corresponding pixel from @img₁@ and @img₂@.
or ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
or = bitwiseImageOp (.|.)


-- TODO
-- invert ∷ Image PixelRGBA8 → Image PixelRGBA8
-- invert img = undefined

-- TODO
-- bitshift ∷ Image PixelRGBA8 → Image PixelRGBA8
-- bitshift img = undefined
