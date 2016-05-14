import Test.Hspec
import Test.QuickCheck
import Data.List
import Chapter2 ( fib,
                  isSorted,
                  curry,
                  uncurry,
                  compose
                     )
import Prelude hiding(curry, uncurry, compose)
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
   it "works for numbers increasing and decreasing order " $ do
    isSorted [1, 2, 1, 2] (<) `shouldBe` False
   it "works for sorted lists of strings in ascending order" $ do
    isSorted (map (show) [1..9]) (<) `shouldBe` True
   it "works for sorted lists of strings in descending order" $ do
    isSorted (map (show) [9,8..1]) (>) `shouldBe` True
   it "works for unsorted lists of strings" $ do
    isSorted ["A", "c", "d"] (>) `shouldBe` False

  describe "curry" $ do
   it "converts function f(A, B) -> C in f(A) -> (B -> C) " $ do
   (Chapter2.curry f) 2 3 `shouldBe` 5


  describe "uncurry" $ do
   it "converts function f(A -> B -> C) in f((A, B) -> C) " $ do
   (Chapter2.uncurry g)(2, 3) `shouldBe` 5

  describe "compose" $ do
     it "converts function f(A -> B) and g(B -> C) in h(A -> C) " $ do
     (Chapter2.compose reverse sort)[2, 5, 3]  `shouldBe` [5, 3, 2]

   where f (a, b) = a + b
         g  a b = a + b

