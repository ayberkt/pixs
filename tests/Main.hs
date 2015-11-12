{-# LANGUAGE FlexibleInstances #-}

module Main where
import Test.Hspec
import Test.QuickCheck
import Transformations
import Data.Word (Word8)
import Codec.Picture (PixelRGBA8(..), Image(..))
import qualified Data.Vector.Storable as VS

instance Arbitrary (Image PixelRGBA8) where
  arbitrary = do
    l <- listOf (arbitrary :: Gen Word8)
    w <- (arbitrary :: Gen Int)
    h <- (arbitrary :: Gen Int)
    return $ Image { imageWidth = w
                   , imageHeight = h
                   , imageData = VS.fromList l}



main :: IO ()
main = hspec $ do
  describe "Plus" $ do
    it "-> is commutative" $ property $
      False
