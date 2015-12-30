{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Arithmetic where

import           Codec.Picture ( PixelRGBA8(..)
                               , Image(..)
                               , Pixel
                               , pixelMap
                               , imageHeight
                               , imageWidth
                               , pixelAt
                               , generateImage)

import qualified Pixs.Transformation as T

-- | Takes as input two identically sized images `img₁` and `img₂`, and produces
-- a new image by summing each pixel of `img₁` with the corresponding pixel
-- from `img₂`.
add ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
add img₁ img₂ = let sum x y = (pixelAt img₁ x y) + (pixelAt img₂ x y)
                in generateImage sum (imageWidth img₁) (imageHeight img₁)

-- | Takes as input two identically sized images `img₁` and `img₂`, and
-- produces a new image by subtracting the pixel at `img₂` from the
-- corresponding one at `img₂`.
subtract ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
subtract img₁ img₂ = let sub x y = (pixelAt img₁ x y) - (pixelAt img₂ x y)
                     in generateImage sub (imageWidth img₁) (imageHeight img₂)

-- TODO
multiply ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
multiply img₁ img₂ = undefined

-- TODO
divide ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
divide img₁ img₂ = undefined

-- TODO
blend ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
blend img₁ img₂ = undefined

-- TODO
and ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
and img₁ img₂ = undefined

-- TODO
or ∷ Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8
or img₁ img₂ = undefined

-- TODO
invert ∷ Image PixelRGBA8 → Image PixelRGBA8
invert img = undefined

-- TODO
bitshift ∷ Image PixelRGBA8 → Image PixelRGBA8
bitshift img = undefined
