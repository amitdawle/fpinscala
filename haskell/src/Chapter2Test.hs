import Test.Hspec
import Test.QuickCheck
import Chapter2 ( fib,
                  isSorted
                     )

main = hspec $
 describe "Chapter2" $ do
  describe "fib" $ do
   it "works for first 10 numbers" $ do
    map (fib) [1..10] `shouldBe` [Just 0, Just 1, Just 1, Just 2, Just 3, Just 5, Just 8, Just 13, Just 21, Just 34]
   it "returns Nothing for negative numbers" $ do
    fib (-1) `shouldBe` Nothing


  describe "isSorted" $ do
   it "works for sorted lists of int in ascending order" $ do
    isSorted [1..10] (<) `shouldBe` True
   it "works for sorted lists of int in descending order" $ do
    isSorted [10,9..1] (>) `shouldBe` True
   it "works for unsorted lists of int in no order" $ do
    isSorted (2:[10,9..1]) (>) `shouldBe` False
   it "works for sorted lists of strings in ascending order" $ do
    isSorted (map (show) [1..9]) (<) `shouldBe` True
   it "works for sorted lists of strings in descending order" $ do
    isSorted (map (show) [9,8..1]) (>) `shouldBe` True
   it "works for unsorted lists of strings" $ do
    isSorted ["A", "c", "d"] (>) `shouldBe` False

