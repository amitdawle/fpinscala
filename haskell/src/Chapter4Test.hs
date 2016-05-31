import Test.Hspec
import Test.QuickCheck
import Prelude hiding(map, filter, sequence)
import Data.List (foldl')
import Chapter4 (Option(None), Option(Some), map, flatMap, getOrElse, orElse, filter,
                 variance, map2, map3, sequence)

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
   it "works for filter ( x > 0) and Some(3)" $ do
    filter (\x -> x > 0) (Some 3) `shouldBe` (Some 3)
   it "works for for filter ( x > 0) and Some(-3)  (returns None)" $ do
    filter (\x -> x > 0)  (Some (-3)) `shouldBe` (None)


  describe "variance" $ do
   it "works for empty List" $ do
    variance [] `shouldBe` (None)
   it "returns 0 [2,2,2]" $ do
    variance [2,2,2] `shouldBe` (Some 0)
   it "returns 0.66 [2,3,4]" $ do
    variance [2,3,4] `shouldSatisfy` (\(Some x) -> abs(x - 0.666) <= 0.001)

  describe "map2" $ do
   it "works when both arguments are None" $ do
     map2  (+) (None) (None)  `shouldBe` (None)
   it "works when first argument is None" $ do
     map2 (+) (Some 2) (None)  `shouldBe` (None)
   it "works when second argument is None" $ do
     map2 (+) (None) (Some 2)  `shouldBe` (None)
   it "works when the arguments have a value" $ do
     map2 (+) (Some 2) (Some 3)  `shouldBe` (Some 5)


  describe "map3" $ do
   it "works when all arguments are None" $ do
     map3 (\x y z -> x + y + z) (None) (None) (None) `shouldBe` (None)
   it "works when first arguments is None" $ do
     map3 (\x y z -> x + y + z) (None) (Some 2) (Some 3)  `shouldBe` (None)
   it "works when first argument is not None but other arguments are None" $ do
     map3 (\x y z -> x + y + z) (Some 2) (None) (None) `shouldBe` (None)
   it "works when second argument is None" $ do
     map3 (\x y z -> x + y + z) (Some 2) (None) (Some 3)  `shouldBe` (None)
   it "works when second argument is not None but other arguments are None" $ do
     map3 (\x y z -> x + y + z) (None) (Some 2) (None)  `shouldBe` (None)
   it "works when third argument is None" $ do
     map3 (\x y z -> x + y + z) (Some 2) (Some 2) (None) `shouldBe` (None)
   it "works when third argument is not None but other arguments are None" $ do
     map3 (\x y z -> x + y + z) (None) (None) (Some 2) `shouldBe` (None)
   it "works when all arguments have a value" $ do
     map3 (\x y z -> x + y + z) (Some 3) (Some 4) (Some 5) `shouldBe` (Some 12)

  describe "sequence" $ do
   it "works for [] list" $ do
    sequence ([] :: (Num a) => [Option a]) `shouldBe` (Some [])
   it "works for list of Somes" $ do
    sequence [Some 2, Some 3, Some 4] `shouldBe` (Some [2,3,4])
   it "works for list of Nones" $ do
    sequence [None :: (Num a) => Option a, None, None] `shouldBe` (None)
   it "works for list of Somes and Nones" $ do
    sequence [Some 3, None, Some 2] `shouldBe` (None)
