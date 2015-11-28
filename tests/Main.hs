{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Word (Word8)
import Codec.Picture ( PixelRGBA8(..)
                     , Image(..)
                     , pixelAt)
import qualified Transformations as T
import qualified Data.Vector.Storable as VS
import Control.Monad (replicateM)

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = do
    Positive size ← arbitrary ∷ Gen (Positive Int)
    pixs          ← listOfSize size
    Positive w    ← arbitrary ∷ Gen (Positive Int)
    let w' = w `rem` size-1
    return Image { imageWidth  = w'
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
prop_double_flip_ID img = if imageWidth img >= 0 && imageHeight img >= 0
                          then T.flipVertical (T.flipVertical  img) == img
                          else True

double_apply_ID :: (Image PixelRGBA8 -> Image PixelRGBA8)
                -> Image PixelRGBA8
                -> Bool
double_apply_ID f img = if imageWidth img >= 0 && imageHeight img >= 0
                        then (f $ f img) == img
                        else True

prop_pixel_add_comm ∷ PixelRGBA8 → PixelRGBA8 → Bool
prop_pixel_add_comm p₁ p₂ = p₁ + p₂ == p₂ + p₁

prop_pixel_add_assoc ∷ PixelRGBA8 → PixelRGBA8 → PixelRGBA8 → Bool
prop_pixel_add_assoc p₁ p₂ p₃ = (p₁ + p₂) + p₃ == p₁ + (p₂ + p₃)


prop_change_red_ID ∷ Int → Image PixelRGBA8 → Bool
prop_change_red_ID x img = if (imageWidth img) >= 0 && (imageHeight img) >= 0
                           then T.changeRed x (T.changeRed (-x) img) == img
                           else True

prop_red_correct ∷ Int → Positive Int → Positive Int → Image PixelRGBA8 → Bool
prop_red_correct a (Positive x) (Positive y) img
  = if (imageWidth img)  >= 0 && (imageHeight img) >= 0
    then let (PixelRGBA8 r _ _ _)  = pixelAt img x y
             newImg                = T.changeRed a img
             (PixelRGBA8 r' _ _ _) = pixelAt newImg x y
         in r' == (r `T.safeAdd` x)
    else True

main ∷ IO ()
main = hspec $ do
  describe "Image equality" $ do
    it "is reflexive" $ property
      prop_reflexivity
  describe "flipVertical" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.flipVertical
  describe "flipHorizontal" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.flipHorizontal
  describe "flip" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.flip
  describe "Pixel addition" $ do
    it "is commutative" $ property
      prop_pixel_add_comm
    it "is associative" $ property
      prop_pixel_add_assoc
    it "correctly adds two arbitrary pixels" $
      let p₁ = PixelRGBA8 20 20 20 20
          p₂ = PixelRGBA8 30 30 30 30
      in p₁ + p₂ `shouldBe` PixelRGBA8 50 50 50 30
    it "handles overflow" $
      let p₁ = PixelRGBA8 250 250 250 250
          p₂ = PixelRGBA8 20  20  20  20
      in p₁ + p₂ `shouldBe` PixelRGBA8 255 255 255 250
  describe "Pixel subtraction" $
    it "handles underflow" $
      let p₁ = PixelRGBA8 5 5 5 5
          p₂ = PixelRGBA8 20  20  20  20
      in p₁ - p₂ `shouldBe` PixelRGBA8 0 0 0 20
  describe "Pixel negation" $
    it "handles normal case" $
      let p = PixelRGBA8 250 250 250 250
      in negate p `shouldBe` PixelRGBA8 5 5 5 255
  describe "Red adjustment" $ do
    it "is correct" $ property $
      prop_red_correct
    it "Gives ID when applied twice with x and -x" $ property $
      prop_change_red_ID
  describe "Field addition" $ do
    it "handles normal cases" $
      let p = PixelRGBA8 5 5 5 5
      in (40 `T.fieldAdd` p) `shouldBe` PixelRGBA8 45 45 45 5
    it "handles overflow" $
      let p = PixelRGBA8 250 250 250 250
      in (50 `T.fieldAdd` p) `shouldBe` PixelRGBA8 255 255 255 250
    it "handles underflow" $
      let p = PixelRGBA8 5 5 5 5
      in ((-50) `T.fieldAdd` p) `shouldBe` PixelRGBA8 0 0 0 5
