{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}

module Transformations where


import Data.Word
import Codec.Picture( PixelRGBA8(..)
                    , Image(..)
                    , Pixel
                    , pixelMap
                    , imageHeight
                    , imageWidth
                    , pixelAt
                    , generateImage)

instance Num PixelRGBA8 where


  negate (PixelRGBA8 r g b _) = PixelRGBA8 r' g' b' 255
    where r' = 255 - r
          g' = 255 - g
          b' = 255 - b


  (PixelRGBA8 r1 g1 b1 _) + (PixelRGBA8 r2 g2 b2 _) = (PixelRGBA8 r g b 255)
    where r' = (fromIntegral r1 + fromIntegral r2) :: Int
          g' = (fromIntegral g1 + fromIntegral g2) :: Int
          b' = (fromIntegral b1 + fromIntegral b2) :: Int
          r  = fromIntegral $ min 255 r' :: Word8
          g  = fromIntegral $ min 255 g' :: Word8
          b  = fromIntegral $ min 255 b' :: Word8

  (PixelRGBA8 r1 g1 b1 _) - (PixelRGBA8 r2 g2 b2 _) = PixelRGBA8 r g b 255
    where r' = (fromIntegral r1 - fromIntegral r2) :: Int
          g' = (fromIntegral g1 - fromIntegral g2) :: Int
          b' = (fromIntegral b1 - fromIntegral b2) :: Int
          r  = fromIntegral $ max 0 r' :: Word8
          g  = fromIntegral $ max 0 g' :: Word8
          b  = fromIntegral $ max 0 b' :: Word8

  _ * _ = undefined
  abs _ = undefined
  fromInteger _ = undefined
  signum _ = undefined

-- TODO: this will no longer be useful when pixel
-- algebra is fully implemented.
adjustBy :: (Num a, Integral a) => Word8 -> a -> Word8
adjustBy x amount
  | newVal > 255 = 255
  | newVal < 0   = 0
  | otherwise    = newVal
  where newVal   = fromIntegral (x + fromIntegral amount)

changeBrightness :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
changeBrightness amount = pixelMap changeBrightness'
  where changeBrightness' (PixelRGBA8 r g b a) = PixelRGBA8 r' g' b' a
          where r' = r `adjustBy` amount
                g' = g `adjustBy` amount
                b' = b `adjustBy` amount

changeRed :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
changeRed amount = pixelMap changeRed'
  where changeRed' (PixelRGBA8 r g b a) = PixelRGBA8 (r `adjustBy` amount) g b a

changeGreen :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
changeGreen amount = pixelMap changeGreen'
  where changeGreen' (PixelRGBA8 r g b a) = PixelRGBA8 r g' b a
                                            where g' = g `adjustBy` amount

flipVertical :: Pixel a => Image a -> Image a
flipVertical img =  generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img x ((imageHeight img) - y - 1)

flipHorizontal :: Pixel a => Image a -> Image a
flipHorizontal img = generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img (imageWidth img - x - 1) y

flip :: Pixel a => Image a -> Image a
flip = flipVertical . flipHorizontal

-- TODO: Do not attempt to implement this without
-- being comfortable with pixel algebra
blur :: Image PixelRGBA8 -> Image PixelRGBA8
blur _ = undefined
