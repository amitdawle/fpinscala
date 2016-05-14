import Test.Hspec
import Test.QuickCheck
import Prelude hiding(tail,
                      dropWhile,
                      takeWhile,
                      drop)

import Chapter3 (List(Empty) ,
                 List(Cons),
                 tail,
                 setHead,
                 dropWhile,
                 takeWhile,
                 drop)

main = hspec $
 describe "Chapter3" $ do


  describe "tail" $ do
   it "works for empty List" $ do
    tail (Empty) `shouldBe` (Empty :: (Num a) => List a )
   it "should work for List with one element" $ do
    tail (Cons 1 Empty) `shouldBe` (Empty :: (Num a) => List a )
   it "should work for List with more than one element of type int" $ do
    tail (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 1 Empty)
   it "should work for List with more than one element of type String" $ do
    tail (Cons "2" (Cons "1" Empty)) `shouldBe` (Cons "1" Empty)
   it "should work for List of List with one element" $ do
    tail (Cons "2" Empty) `shouldBe` (Empty )
   it "should work for List of List" $ do
    tail (Cons (Cons "1" Empty) Empty) `shouldBe` (Empty)
   it "should work for List of List with many elements" $ do
    tail (Cons (Cons "2" Empty) (Cons (Cons "1" Empty) Empty) ) `shouldBe` (Cons (Cons "1" Empty) Empty )


  describe "setHead" $ do
   it "works for empty List" $ do
    setHead 1 (Empty) `shouldBe` (Cons 1 Empty)
   it "should work for List with one element" $ do
    setHead 2 (Cons 1 Empty) `shouldBe` (Cons 2 Empty)
   it "should work for List with more than one element" $ do
    setHead 3 (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 3 (Cons 1 Empty))

  describe "drop" $ do
   it "works for empty List" $ do
    drop 100 (Empty) `shouldBe` (Empty :: (Num a) => List a)
   it "should drop all elements for List when n > number of elements in List" $ do
    drop 100 (Cons 1 Empty) `shouldBe` (Empty)
   it "should drop 2 elements for List with 3 elements when n is 2" $ do
    drop 2 (Cons 3(Cons 2 (Cons 1 Empty))) `shouldBe` (Cons 1 Empty)
   it "should drop no element when n < 0" $ do
    drop (-1) (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 2 (Cons 1 Empty))


  describe "dropWhile" $ do
   it "works for empty List" $ do
    dropWhile (>0) (Empty) `shouldBe` (Empty)
   it "should drop all elements for List with one element satisfying the predicate" $ do
    dropWhile (>0) (Cons 1 Empty) `shouldBe` (Empty)
   it "should drop all elements for List with all elements satisfying the predicate" $ do
    dropWhile (>0) (Cons 2 (Cons 1 Empty)) `shouldBe` (Empty)
   it "should drop no element for List where no element satisfies the predicate" $ do
    dropWhile (<0) (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 2 (Cons 1 Empty))
   it "should drop 2 element for List of 4 where first 2 elements satisfies the predicate" $ do
    dropWhile (>0) (Cons 2 (Cons 1(Cons 0 (Cons (-1) Empty)))) `shouldBe` (Cons 0 (Cons (-1) Empty))
   it "should drop only 2 element for List of 6 where elements (0,1,4,5) satisfy the predicate but (2,3) don't" $ do
    dropWhile (>0) (Cons 2 (Cons 1(Cons 0 (Cons (-1) (Cons 2 (Cons 1 Empty)))))) `shouldBe` (Cons 0 (Cons (-1) (Cons 2 (Cons 1 Empty))))


  describe "takeWhile" $ do
   it "works for empty List" $ do
    takeWhile (>0) (Empty) `shouldBe` (Empty)
   it "should take all elements for List with one element satisfying the predicate" $ do
    takeWhile (>0) (Cons 1 Empty) `shouldBe` (Cons 1 Empty)
   it "should take all elements for List with all elements satisfying the predicate" $ do
    takeWhile (>0) (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 2 (Cons 1 Empty))
   it "should take no element for List where no element satisfies the predicate" $ do
    takeWhile (<0) (Cons 2 (Cons 1 Empty)) `shouldBe` (Empty)
   it "should take 2 element for List of 4 where first 2 elements satisfies the predicate" $ do
    takeWhile (>0) (Cons 2 (Cons 1(Cons 0 (Cons (-1) Empty)))) `shouldBe` (Cons 2 (Cons 1 Empty))
   it "should take only 2 element for List of 6 where elements (0,1,4,5) satisfy the predicate but (2,3) don't" $ do
    takeWhile (>0) (Cons 2 (Cons 1(Cons 0 (Cons (-1) (Cons 2 (Cons 1 Empty)))))) `shouldBe` (Cons 2 (Cons 1 Empty))
