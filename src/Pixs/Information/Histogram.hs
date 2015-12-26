{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Information.Histogram where

import           Codec.Picture                             (Image,
                                                            PixelRGBA8 (..),
                                                            imageHeight,
                                                            imageWidth, pixelAt)
import           Data.Colour
import           Data.Colour.Names
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as M
import           Data.Word                                 (Word8)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           Prelude                                   hiding (lookup)

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

greenCount ∷ Image PixelRGBA8 → Map Word8 Int
greenCount img = colorCount img Green

fill ∷ String → Colour Double → [(α, (β, β))] → EC θ (PlotFillBetween α β)
fill title color vs = liftEC $ do
  plot_fillbetween_title .= title
  let color' = withOpacity color 0.7
  plot_fillbetween_style  .= solidFillStyle color'
  plot_fillbetween_values .= vs

-- | Create the histogram and save it to a file.
makeHistogram ∷ Image PixelRGBA8 → IO ()
makeHistogram img = let toDouble ∷ Integral α ⇒ (α,β) → (Double,β)
                        toDouble (x,y) = (fromIntegral x,y)
                        [rCount,gCount,bCount] =   map toDouble
                                                 . M.toList
                                                 . colorCount img
                                                 <$> [Red, Green, Blue]
                    in toFile def "example.svg" $ do
                         layout_title .= "Color histogram"
                         layout_title_style . font_size .= 15
                         plot (fill "Red" red [(d, (0, v))
                                              | (d, v) ← rCount])
                         plot (fill "Green" green [(d, (0, v))
                                                  | (d, v) ← gCount])
                         plot (fill "Blue" blue [(d, (0, v))
                                                  | (d, v) ← bCount])
