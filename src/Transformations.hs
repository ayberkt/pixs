module Transformations where

import Control.Applicative
import Data.Word (Word8(..))
import Codec.Picture(DynamicImage(..), PixelRGBA8(..), Image(..), pixelMap
                    ,imageHeight, imageWidth, pixelAt, pixelBaseIndex, generateImage)

adjustBy :: Word8 -> Int -> Word8
adjustBy x amount
  | newVal > 255 = 255
  -- | newVal < 0   = fromIntegral 0
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

flipVertical :: Image PixelRGBA8 -> Image PixelRGBA8
flipVertical img =  generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img x ((imageHeight img) - y - 1)

flipHorizontal :: Image PixelRGBA8 -> Image PixelRGBA8
flipHorizontal img = generateImage complement (imageWidth img) (imageHeight img)
  where complement x y = pixelAt img (imageWidth img - x - 1) y

flip :: Image PixelRGBA8 -> Image PixelRGBA8
flip = flipVertical . flipHorizontal
