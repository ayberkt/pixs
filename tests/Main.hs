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
                     , pixelAt
                     , PixelBaseComponent(..))
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


deriving instance Eq (Image PixelRGBA8)
deriving instance Show (Image PixelRGBA8)

prop_reflexivity :: Image PixelRGBA8 -> Bool
prop_reflexivity img = img == img

prop_double_flip_ID :: Image PixelRGBA8 -> Bool
prop_double_flip_ID img = if (imageWidth img) >= 0 && (imageHeight img) >= 0
                          then (T.flipVertical (T.flipVertical  img)) == img
                          else True
  where pixsLength = (imageWidth img) * (imageHeight img)

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

main :: IO ()
main = hspec $ do
  describe "Image equality" $ do
    it "is reflexive" $ property $
      prop_reflexivity
  describe "flipVertical" $ do
    it "gives identity when applied twice" $ property $
      prop_double_flip_ID
