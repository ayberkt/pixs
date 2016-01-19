{-# LANGUAGE  FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Word               (Word8)
import           Codec.Picture           ( PixelRGBA8(..)
                                         , Image(..)
                                         , pixelAt)
import           Pixs.Operations.Pixel   ((⊕))
import qualified Pixs.Transformation     as T
import qualified Data.Vector.Storable    as VS
import qualified Pixs.Operations.Image   as A
import           Control.Monad           (replicateM)

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

prop_double_reflect_ID ∷ Image PixelRGBA8 → Bool
prop_double_reflect_ID img = if imageWidth img >= 0 && imageHeight img >= 0
                          then T.reflectVertical (T.reflectVertical  img) == img
                          else True

double_apply_ID ∷ (Image PixelRGBA8 -> Image PixelRGBA8)
                → Image PixelRGBA8
                → Bool
double_apply_ID f img = if imageWidth img >= 0 && imageHeight img >= 0
                        then (f $ f img) == img
                        else True

prop_pixel_comm ∷ (PixelRGBA8 → PixelRGBA8 → PixelRGBA8)
                → PixelRGBA8
                → PixelRGBA8
                → Bool
prop_pixel_comm op p₁ p₂ = p₁ `op` p₂ == p₂ `op` p₁

prop_pixel_assoc ∷ (PixelRGBA8 → PixelRGBA8 → PixelRGBA8)
                 → PixelRGBA8
                 → PixelRGBA8
                 → PixelRGBA8
                 → Bool
prop_pixel_assoc op p₁ p₂ p₃ = (p₁ `op` p₂) `op` p₃ == p₁ `op` (p₂ `op` p₃)

prop_pixel_dist ∷ (PixelRGBA8 → PixelRGBA8 → PixelRGBA8)
                → (PixelRGBA8 → PixelRGBA8 → PixelRGBA8)
                → PixelRGBA8
                → PixelRGBA8
                → PixelRGBA8
                → Bool
prop_pixel_dist op₁ op₂ p₁ p₂ p₃ =
     p₁ `op₁` (p₂ `op₂` p₃) == (p₁ `op₁` p₂) `op₂` (p₁ `op₁` p₃)
  && (p₂ `op₂` p₃) `op₁` p₁ == (p₂ `op₁` p₁) `op₂` (p₃ `op₁` p₁)

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
         in r' == (r ⊕ x)
    else True

sides ∷ [Image a] → [Int]
sides = (>>= \x → [imageWidth x, imageHeight x])

prop_image_comm ∷ (Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8)
                → Image PixelRGBA8
                → Image PixelRGBA8
                → Bool
prop_image_comm op img₁ img₂ =
     (any (< 0) $ sides [img₁, img₂])
  || (img₁ `op` img₂) == (img₂ `op` img₁)

prop_image_assoc ∷ (Image PixelRGBA8 → Image PixelRGBA8 → Image PixelRGBA8)
                 → Image PixelRGBA8
                 → Image PixelRGBA8
                 → Image PixelRGBA8
                 → Bool
prop_image_assoc op img₁ img₂ img₃ =
     (any (< 0) $ sides [img₁, img₂, img₃])
  || (img₁ `op` (img₂ `op` img₃)) == ((img₁ `op` img₂) `op` img₃)

main ∷ IO ()
main = hspec $ do
  describe "Image equality" $ do
    it "is reflexive" $ property
      prop_reflexivity
  describe "reflectVertical" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.reflectVertical
  describe "reflectHorizontal" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.reflectHorizontal
  describe "reflect" $ do
    it "gives identity when applied twice" $ property $
      double_apply_ID T.reflect
  describe "Pixel addition" $ do
    it "is commutative" $ property $
      prop_pixel_comm (+)
    it "is associative" $ property $
      prop_pixel_assoc (+)
    it "correctly adds two arbitrary pixels" $
      let p₁ = PixelRGBA8 21 21 21 21
          p₂ = PixelRGBA8 30 30 30 30
      in p₁ + p₂ `shouldBe` PixelRGBA8 51 51 51 30
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
  describe "Pixel multiplication" $ do
    it "is commutative" $ property $
      prop_pixel_comm (*)
    it "is associative" $ property $
      prop_pixel_assoc (*)
    it "is distributive over addition" $ property $
      prop_pixel_dist (*) (+)
  describe "Image addition" $ do
    it "is commutative" $ property $
      prop_image_comm A.add
    it "is associative" $ property $
      prop_image_assoc A.add
  describe "Image multiplication" $ do
    it "is commutative" $ property $
      prop_image_comm A.multiply
    it "is associative" $ property $
      prop_image_assoc A.multiply
  describe "Red adjustment" $ do
    it "is correct" $ property $
      prop_red_correct
    it "Gives ID when applied twice with x and -x" $ property $
      prop_change_red_ID
  describe "Negation" $ do
    let prop_double_neg_ID ∷ Image PixelRGBA8 → Bool
        prop_double_neg_ID img = if imageWidth img >= 0 && imageHeight img >= 0
                                 then let img' = T.negateImage . T.negateImage $ img
                                      in img' == img
                                 else True
    it "gives ID when applied twice." $ property $
      prop_double_neg_ID
