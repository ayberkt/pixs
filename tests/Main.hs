{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , StandaloneDeriving #-}

module Main where
import Test.Hspec
import Test.QuickCheck
import Data.Word (Word8)
import Codec.Picture (PixelRGBA8(..), Image(..), pixelAt, PixelBaseComponent(..))
import Control.Applicative
import qualified Transformations as T
import qualified Data.Vector.Storable as VS

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = do
    l <- listOf (arbitrary :: Gen Word8)
    w <- (arbitrary :: Gen Int)
    h <- (arbitrary :: Gen Int)
    return $ Image { imageWidth  = w
                   , imageHeight = h
                   , imageData   = VS.fromList l
                   }


deriving instance Eq (Image PixelRGBA8)
deriving instance Show (Image PixelRGBA8)

-- instance Eq (Image PixelRGBA8) where
--   img1 == img2
--     | (width1 < 0) || (width2 < 0)
--       = True
--     | (height1 < 0) || (height2 < 0)
--       = True
--     | (width1 == 0) && (width2 == 0)
--       = True
--     | (height1 == 0) && (height2 == 0)
--       = True
--     | width1 /= width2
--       = False
--     | height1 /= height2
--       = False
--     | otherwise = all id [(foo img1 img2 x y) | x <- [0..width1]
--                                               , y <- [0..height1]]
--     where width1  = imageWidth  img1
--           width2  = imageWidth  img2
--           height1 = imageHeight img1
--           height2 = imageHeight img2
--           foo p q x y = (pixelAt p x y) == (pixelAt q x y)

reflexivity :: Image PixelRGBA8 -> Bool
reflexivity img = img == img

doubleFlipIsID :: Image PixelRGBA8 -> Bool
doubleFlipIsID img = (T.flip (T.flip  img)) == img

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

main :: IO ()
main = hspec $ do
  describe "Image equality" $ do
    it "is reflexive" $ property $
      reflexivity
  describe "Flip" $ do
    it "gives identity when applied twice" $ property $
      doubleFlipIsID
