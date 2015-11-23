{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Word (Word8)
import Codec.Picture ( PixelRGBA8(..)
                     , Image(..))
import qualified Transformations as T
import qualified Data.Vector.Storable as VS
import Control.Monad (replicateM)

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = do
    Positive size ← (arbitrary ∷ Gen (Positive Int))
    pixs          ← listOfSize size
    Positive w    ← arbitrary ∷ Gen (Positive Int)
    let w' = w `rem` size-1
    return $ Image { imageWidth  = w'
                   , imageHeight = w' `div` size-1
                   , imageData   = VS.fromList pixs
                   }

listOfSize ∷ Int → Gen [Word8]
listOfSize x = fmap concat $ replicateM x genPixel

genPixel ∷ Gen [Word8]
genPixel = do
     a ← arbitrary ∷ Gen Word8
     b ← arbitrary ∷ Gen Word8
     c ← arbitrary ∷ Gen Word8
     d ← arbitrary ∷ Gen Word8
     return [a,b,c,d]

instance Arbitrary PixelRGBA8 where
  arbitrary = do
    r ← arbitrary ∷ Gen Word8
    g ← arbitrary ∷ Gen Word8
    b ← arbitrary ∷ Gen Word8
    a ← arbitrary ∷ Gen Word8
    return $ PixelRGBA8 r g b a


deriving instance Eq (Image PixelRGBA8)
deriving instance Show (Image PixelRGBA8)

prop_reflexivity ∷ Image PixelRGBA8 → Bool
prop_reflexivity img = img == img

prop_double_flip_ID ∷ Image PixelRGBA8 → Bool
prop_double_flip_ID img = if (imageWidth img) >= 0 && (imageHeight img) >= 0
                          then (T.flipVertical (T.flipVertical  img)) == img
                          else True

prop_pixel_add_comm ∷ PixelRGBA8 → PixelRGBA8 → Bool
prop_pixel_add_comm p₁ p₂ = p₁ + p₂ == p₂ + p₁

prop_pixel_add_assoc ∷ PixelRGBA8 → PixelRGBA8 → PixelRGBA8 → Bool
prop_pixel_add_assoc p₁ p₂ p₃ = (p₁ + p₂) + p₃ == p₁ + (p₂ + p₃)

main ∷ IO ()
main = hspec $ do
  describe "Image equality" $ do
    it "is reflexive" $ property $
      prop_reflexivity
  describe "flipVertical" $ do
    it "gives identity when applied twice" $ property $
      prop_double_flip_ID
  describe "Pixel addition" $ do
    it "is commutative" $ property $
      prop_pixel_add_comm
    it "is associative" $ property $
      prop_pixel_add_assoc
    it "correctly adds two arbitrary pixels" $
      let p₁ = PixelRGBA8 20 20 20 20
          p₂ = PixelRGBA8 30 30 30 30
      in p₁ + p₂ `shouldBe` PixelRGBA8 50 50 50 255
    it "handles overflow" $
      let p₁ = PixelRGBA8 250 250 250 250
          p₂ = PixelRGBA8 20  20  20  20
      in p₁ + p₂ `shouldBe` PixelRGBA8 255 255 255 255
  describe "Pixel subtraction" $ do
    it "handles underflow" $
      let p₁ = PixelRGBA8 5 5 5 5
          p₂ = PixelRGBA8 20  20  20  20
      in p₁ - p₂ `shouldBe` PixelRGBA8 0 0 0 255
