{-# LANGUAGE FlexibleInstances #-}

module Main where
import Test.Hspec
import Test.QuickCheck
import Data.Word (Word8)
import Codec.Picture (PixelRGBA8(..), Image(..), pixelAt)
import qualified Transformations as T
import qualified Data.Vector.Storable as VS

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = do
    l <- listOf (arbitrary :: Gen Word8)
    w <- (arbitrary :: Gen Int)
    h <- (arbitrary :: Gen Int)
    return $ Image { imageWidth = w
                   , imageHeight = h
                   , imageData = VS.fromList l}

instance Eq (Image PixelRGBA8) where
  img1 == img2
    | width1 /= width2
      = False
    | height1 /= height2
      = False
    | otherwise = all id [foo x y | x <- [0.. width1-1]
                                  , y <- [0.. height1-1]]
    where width1  = imageWidth  img1
          width2  = imageWidth  img2
          height1 = imageHeight img1
          height2 = imageHeight img2
          foo x y = (pixelAt img1 x y) == (pixelAt img2 x y)

instance Show (Image PixelRGBA8) where
  show _ = ""


doubleFlipIsID :: Image PixelRGBA8 -> Bool
doubleFlipIsID img = (T.flip (T.flip  img)) == img

main :: IO ()
main = hspec $ do
  describe "Double flip" $ do
    it "doesn't modify the image" $ property $
      doubleFlipIsID
