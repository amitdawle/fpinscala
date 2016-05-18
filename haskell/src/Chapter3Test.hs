import Test.Hspec
import Test.QuickCheck
import Prelude hiding(tail,
                      dropWhile,
                      takeWhile,
                      drop,
                      init,
                      length)

import Chapter3 (List(Empty) ,
                 List(Cons),
                 tail,
                 setHead,
                 dropWhile,
                 takeWhile,
                 drop,
                 init,
                 foldRight,
                 length)

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
    takeWhile (>0) (Empty) `shouldBe` (Empty:: (Num a) => List a)
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

  describe "init" $ do
   it "works for empty List" $ do
    init (Empty) `shouldBe` (Empty :: (Num a) => List a)
   it "init on List with one element is Empty" $ do
    init (Cons 1 Empty) `shouldBe` (Empty)
   it "should take all elements except last for List with 2 elements" $ do
    init (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 2 Empty)
   it "should take all element except last for List with more than 2 elements" $ do
    init (Cons 3 (Cons 2 (Cons 1 Empty))) `shouldBe` (Cons 3(Cons 2 Empty))

  describe "foldRight" $ do
   it "works for empty List" $ do
    foldRight (+) 0 (Empty) `shouldBe` 0
   it "can sum a List with one element" $ do
    foldRight (+) 0 (Cons 1 Empty) `shouldBe` 1
   it "can sum a List with more than one element" $ do
    foldRight (+) 0 (Cons 1(Cons 2 Empty)) `shouldBe` 3
   it "can calculate a sum of a large List " $ do
    foldRight (+) 0 (foldr (\x t -> Cons x t) Empty [1..1000]  ) `shouldBe` 500500
   it "can calculate a product of a List with more than one element" $ do
    foldRight (*) 1 (Cons 3 (Cons 4(Cons 2 Empty))) `shouldBe` 24
   it "can calculate a product of a large List " $ do
    foldRight (*) 1 (foldr (\x t -> Cons x t) Empty [1..10]  ) `shouldBe` 3628800 -- 10!
   it "can short circuit a product of an infinite List if one element is 0" $ do
    foldRight (\x y -> if (x == 0) then 0 else x * y) 1 (foldr (\x t -> Cons x t) Empty [0,1..]) `shouldBe` 0

  describe "length with foldRight" $ do
   it "works for empty List" $ do
      length (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      length (Cons 1 Empty) `shouldBe` 1
   it "works for List with many elements" $ do
      length (foldr (\x t -> Cons x t) Empty [1..10]  ) `shouldBe` 10
