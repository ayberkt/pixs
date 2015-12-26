{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Information.Histogram where

import           Codec.Picture ( Image
                               , PixelRGBA8(..)
                               , imageHeight
                               , imageWidth
                               , pixelAt)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (sRGB)
import           Data.Word     (Word8)
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Prelude       hiding (lookup)

data Color = Red
           | Green
           | Blue
           deriving (Show,Eq,Ord)

-- | Takes in an image `img` and a @Color@ `c`. Returns a map that stores
-- for every given Word8 value v, the total number of pixels that hold a
-- a `c` component of magnitude v.
colorCount ∷ Image PixelRGBA8 → Color → M.Map Word8 Int
colorCount img c = let count m x y
                         | y == imageHeight img - 1 = m
                         | x == imageWidth  img - 1 = count m' 0 (succ y)
                         | otherwise = count m' (succ x) y
                         where (PixelRGBA8 r g b _) = pixelAt img x y
                               m' = case c of Red   → M.adjust (+ 1) r m
                                              Green → M.adjust (+ 1) g m
                                              Blue  → M.adjust (+ 1) b m
                       initMap = M.fromList $ map (\x → (x, 0)) [0..255]
                   in count initMap 0 0

redCount ∷ Image PixelRGBA8 → Map Word8 Int
redCount img = colorCount img Red

fill ∷ String → [(α, (β, β))] → EC θ (PlotFillBetween α β)
fill title vs = liftEC $ do
  plot_fillbetween_title .= title
  let color = opaque red
  plot_fillbetween_style  .= solidFillStyle color
  plot_fillbetween_values .= vs

toDouble ∷ Integral α ⇒ (α, β) → (Double, β)
toDouble (x, y) = (fromIntegral x, y)

-- | Create the histogram and save it to a file.
makeHistogram ∷ Image PixelRGBA8 → IO ()
makeHistogram img = let [rCount,_,_] = colorCount img
                                                   <$> [Red, Green, Blue]
                        rCount' = toDouble <$> M.toList rCount
                    in toFile def "example.svg" $ do
                         layout_title .= "Color histogram"
                         layout_title_style . font_size .= 10
                         plot (fill "Red" [(d, (0, v))
                                          | (d, v) ← rCount'])
