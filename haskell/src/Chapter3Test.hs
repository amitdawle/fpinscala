import Test.Hspec
import Test.QuickCheck
import Prelude hiding(tail)
--import Data.List hiding (tail)
import Chapter3 (List(Empty) ,
                 List(Cons), tail )

main = hspec $
 describe "Chapter3" $ do
  describe "tail" $ do
   it "works for empty List" $ do
     tail (Empty) `shouldBe` (Empty :: (Num a) => List a )
   it "should work for list with one element" $ do
    tail (Cons 1 Empty) `shouldBe` (Empty :: (Num a) => List a )
   it "should work for list with more than one element of type int" $ do
    tail (Cons 2 (Cons 1 Empty)) `shouldBe` (Cons 1 Empty)
   it "should work for list with more than one element of type String" $ do
    tail (Cons "2" (Cons "1" Empty)) `shouldBe` (Cons "1" Empty)
  it "should work for list of list with one element" $ do
    tail (Cons "2" Empty) `shouldBe` (Empty ::  List [Char] )
  it "should work for list of list" $ do
    tail (Cons (Cons "1" Empty) Empty) `shouldBe` (Empty :: List (List[Char]) )
  it "should work for list of list with many elements" $ do
    tail (Cons (Cons "2" Empty) (Cons (Cons "1" Empty) Empty) ) `shouldBe` (Cons (Cons "1" Empty) Empty )


