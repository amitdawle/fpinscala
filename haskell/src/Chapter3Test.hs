import Test.Hspec
import Test.QuickCheck
import Prelude hiding(tail,
                      dropWhile,
                      takeWhile,
                      drop,
                      init,
                      length,
                      sum,
                      product,
                      reverse)
import Data.List (foldl')
import Chapter3 (List(Empty) ,
                 List(Cons),
                 tail,
                 setHead,
                 dropWhile,
                 takeWhile,
                 drop,
                 init,
                 foldRight,
                 length,
                 foldLeft,
                 sum,
                 product,
                 length2,
                 reverse,
                 foldLeftWithFoldRight,
                 foldRightWithFoldLeft,
                 append)

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


  describe "foldLeft" $ do
   it "works for empty List" $ do
      foldLeft (\t x -> x + t) (0) (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      foldLeft (\t x -> x + t) (0) (Cons 1 Empty) `shouldBe` 1
   it "works for List with two elements" $ do
      foldLeft (\t x -> x + t) (0) (Cons 1 (Cons 2 Empty)) `shouldBe` 3
 -- foldLeft is strict
   it "works for large List with 100000 elements" $ do
     foldLeft (\t x -> x + t) (0) (foldl' (\t x -> Cons x t) Empty [1..100000]) `shouldBe` 5000050000


  describe "sum" $ do
   it "works for empty List" $ do
      sum  (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      sum (Cons 1 Empty) `shouldBe` 1
   it "works for large List with 1000 elements" $ do
      sum (foldl' (\t x -> Cons x t) Empty [1..1000]) `shouldBe` 500500


  describe "product" $ do
   it "works for empty List" $ do
      product (Empty) `shouldBe` 1
   it "works for List with one element" $ do
      product (Cons 1 Empty) `shouldBe` 1
   it "works for large List with 10 elements" $ do
      product (foldl' (\t x -> Cons x t) Empty [1..10]) `shouldBe` 3628800


  describe "lengthWithFoldLeft" $ do
   it "works for empty List" $ do
      length2 (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      length2 (Cons 1 Empty) `shouldBe` 1
   it "works for large List with 1000 elements" $ do
      length2 (foldl' (\t x -> Cons x t) Empty [1..1000]) `shouldBe` 1000


  describe "reverse" $ do
   it "works for empty List" $ do
      reverse (Empty :: (Num a) => List a) `shouldBe` Empty
   it "works for List with one element" $ do
      reverse (Cons 1 Empty) `shouldBe` (Cons 1 Empty)
   it "works for List with more than element" $ do
      reverse (Cons 1 (Cons 2 (Cons 3 Empty))) `shouldBe` (Cons 3 (Cons 2 (Cons 1 Empty)))


  describe "foldLeftWithFoldRight" $ do
   it "works for empty List" $ do
      foldLeftWithFoldRight (\t x -> x + t) 0 (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      foldLeftWithFoldRight (\t x -> x + t) 0 (Cons 1 Empty) `shouldBe` 1
   it "works for large List with 1000 elements" $ do
      foldLeftWithFoldRight (\t x -> x + t) 0 (foldl' (\t x -> Cons x t) Empty [1..1000]) `shouldBe` 500500
   it "does reverses the list " $ do
      foldLeftWithFoldRight (\t x -> Cons x t) Empty (Cons 1 (Cons 2 (Cons 3 Empty))) `shouldBe` (Cons 3 (Cons 2 (Cons 1 Empty)))


  describe "foldRightWithFoldLeft" $ do
   it "works for empty List" $ do
      foldRightWithFoldLeft (\x t -> x + t) 0 (Empty) `shouldBe` 0
   it "works for List with one element" $ do
      foldRightWithFoldLeft (\x t -> x + t) 0 (Cons 1 Empty) `shouldBe` 1
   it "works for List with one element" $ do
      foldRightWithFoldLeft (\x t -> x - t) 0 (Cons 2 (Cons 3 (Cons 4 Empty))) `shouldBe` 3
   it "works for large List with 1000 elements" $ do
      foldRightWithFoldLeft (\x t -> x + t) 0 (foldl' (\t x -> Cons x t) Empty [1..1000]) `shouldBe` 500500
   it "does not reverse the list " $ do
      foldRightWithFoldLeft (\x t -> Cons x t) Empty (Cons 1 (Cons 2 (Cons 3 Empty))) `shouldBe` (Cons 1 (Cons 2 (Cons 3 Empty)))


  describe "append" $ do
   it "works for empty Lists" $ do
      append (Empty::(Num a)=> List a) (Empty::(Num a)=> List a) `shouldBe` (Empty::(Num a)=> List a)
   it "works when first list list is empty" $ do
      append (Empty) (Cons 1 Empty) `shouldBe` (Cons 1 Empty)
   it "works when second list is empty" $ do
      append (Cons 1 Empty) (Empty) `shouldBe` (Cons 1 Empty)
   it "works for one list with one element and other with many" $ do
      append (Cons 1 Empty) (Cons 3 ((Cons 2 Empty))) `shouldBe` (Cons 1 (Cons 3 (Cons 2 Empty)))
      append (Cons 3 ((Cons 2 Empty))) (Cons 1 Empty)  `shouldBe` (Cons 3 (Cons 2 (Cons 1 Empty)))
   it "works for larger Lists" $ do
      append (Cons 3 ((Cons 2 Empty))) (Cons 4 ((Cons 5 Empty)))  `shouldBe` (Cons 3 (Cons 2 (Cons 4 (Cons 5 Empty))))

