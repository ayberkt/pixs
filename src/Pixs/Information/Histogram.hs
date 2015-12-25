{-# LANGUAGE UnicodeSyntax #-}

module Pixs.Information.Histogram where

import           Codec.Picture ( Image
                               , PixelRGBA8(..)
                               , imageHeight
                               , imageWidth
                               , pixelAt)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Word     (Word8)
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Cairo
import           Prelude       hiding (lookup)

data Color = Red
           | Green
           | Blue
           deriving (Show,Eq,Ord)

-- | Takes in an image `img` and a color `c`. Returns a map that stores
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

type Pt = (Word8, Int)
type Points = [Pt]

dataSeries ∷ Image PixelRGBA8 → [(String, Points)]
dataSeries img =
  let [rCount, gCount, bCount] = colorCount img <$> [Red, Green, Blue]
  in [ ("Red",   M.toList rCount)
     , ("Green", M.toList gCount)
     , ("Blue",  M.toList bCount)]

fill title vs = liftEC $ do
  plot_fillbetween_title .= title
  color ← takeColor
  plot_fillbetween_style  .= solidFillStyle color
  plot_fillbetween_values .= vs

toDouble ∷ Integral α ⇒ (α, β) → (Double, β)
toDouble (x, y) = (fromIntegral x, y)

makeHistogram ∷ Image PixelRGBA8 → IO ()
makeHistogram img = let [rCount, gCount, bCount] = colorCount img
                                                   <$> [Red, Green, Blue]
                    in toFile def "example.png" $ do
                      layout_title .= "Color histogram"
                      plot (fill "Red" [(d, (0, v)) | (d, v) ← toDouble <$> (M.toList rCount)])
