module Main where
import Test.Hspec
import Test.QuickCheck
import Transformations

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

main :: IO ()
main = hspec $ do
  describe "Plus" $ do
    it "-> is commutative" $ property $
      plusCommutative
