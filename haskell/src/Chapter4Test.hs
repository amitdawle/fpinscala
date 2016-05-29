import Test.Hspec
import Test.QuickCheck
import Prelude hiding(map, filter)
import Data.List (foldl')
import Chapter4 (Option(None), Option(Some), map, flatMap, getOrElse, orElse, filter)

main = hspec $
 describe "Chapter4" $ do

  describe "map" $ do
   it "works for None" $ do
    map (+1) (None) `shouldBe` (None::(Num a) => Option a)
   it "should work for map (+1) Some(2) => Some(3) " $ do
    map (+1) (Some 2) `shouldBe` (Some 3)
   it "should work for map (*3) Some(2) => Some(6) " $ do
    map (\x -> x*3) (Some 2) `shouldBe` (Some 6)

  describe "flatMap" $ do
   it "works for None" $ do
    flatMap (\x -> if (x > 0) then Some x else None ) (None) `shouldBe` (None)
   it "should work for flatMap (x -> Some(x + 3) ) Some(2) => Some(5)" $ do
    flatMap (\x -> Some(x + 3) ) (Some 2) `shouldBe` (Some 5)

  describe "getOrElse" $ do
   it "works for None" $ do
    getOrElse (None) (None) `shouldBe` (None::(Num a) => Option a)
   it "works for None with return value not None" $ do
    getOrElse (None) (1) `shouldBe` (1)
   it "works for Some" $ do
    getOrElse (Some 2) (1) `shouldBe` (2)

  describe "orElse" $ do
   it "works for None" $ do
    orElse (None) (None) `shouldBe` (None::(Num a) => Option a)
   it "works for None with return value not None" $ do
    orElse (None) (Some 2) `shouldBe` (Some 2)
   it "works for Some" $ do
    orElse (Some 2) (Some 3) `shouldBe` (Some 2)


  describe "filter" $ do
   it "works for None" $ do
    filter (\x -> x > 0) (None) `shouldBe` (None::(Num a) => Option a)
   it "works for for filter ( x > 0) and Some(3)" $ do
    filter (\x -> x > 0) (Some 3) `shouldBe` (Some 3)
   it "works for for filter ( x > 0) and Some(-3)  (returns None)" $ do
    filter (\x -> x > 0)  (Some (-3)) `shouldBe` (None)