import Test.Hspec
import Test.QuickCheck

import Prelude hiding (maximum, map)

import Chapter3b (hasSubSequence, Tree(Leaf), Tree(Branch), size, maximum, depth, map,
                  size2, maximum2, depth2, map2)

main = hspec $
 describe "Chapter3b" $ do

  describe "hasSubSequence" $ do
   it "works when 'super string' is []" $ do
    hasSubSequence [] [2,3,4] `shouldBe` False
   it "works when 'sub string' is []" $ do
    hasSubSequence [2,3,4] [] `shouldBe` False
   it "works when 'sub string' exist in 'super string'" $ do
    hasSubSequence [1,2,3,4,5,6] [2,3,4] `shouldBe` True
   it "works when 'sub string' does not exist in 'super string' " $ do
    hasSubSequence [1,2,3,4,5,6] [0,7]  `shouldBe` False
   it "works correctly (returns false) when only part of 'sub string' exist in 'super string'" $ do
    hasSubSequence [1,2,3,4,5,6] [2,3,7]  `shouldBe` False
   it "works correctly (returns false) when only 'sub string' is bigger than 'super string'" $ do
    hasSubSequence [1,2,3,4,5,6] [1,2,3,4,5,6,7,8,9] `shouldBe` False


  describe "size" $ do
   it "works for Tree with single leaf" $ do
    size (Leaf 1) `shouldBe` 1
   it "works for Tree with branch having two leaves" $ do
    size (Branch (Leaf 1) (Leaf 2)) `shouldBe` 3
   it "works for deeper Tree " $ do
    size (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 7
   it "works for unbalanced trees " $ do
    size (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 4)) `shouldBe` 5

  describe "maximum" $ do
   it "works for Tree with single leaf" $ do
    maximum (Leaf 5) `shouldBe` 5
   it "works for Tree with branch having just two leaves" $ do
    maximum (Branch (Leaf 1) (Leaf 2)) `shouldBe` 2
   it "works for deeper Tree " $ do
    maximum (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 5
   it "works for unbalanced trees " $ do
    maximum (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 4)) `shouldBe` 4


  describe "depth" $ do
   it "works for Tree with single leaf" $ do
    depth (Leaf 5) `shouldBe` 1
   it "works for Tree with branch having just two leaves" $ do
    depth (Branch (Leaf 1) (Leaf 2)) `shouldBe` 2
   it "works for deeper Tree " $ do
    depth (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 3
   it "works for unbalanced trees " $ do
    depth (Branch (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) (Leaf 4)) `shouldBe` 4

  describe "map" $ do
   it "works for Tree with single leaf" $ do
    map (+2) (Leaf 5) `shouldBe` (Leaf 7)
   it "works for Tree with branch having just two leaves" $ do
    map (*3) (Branch (Leaf 1) (Leaf 2)) `shouldBe` (Branch (Leaf 3) (Leaf 6))
   it "works for deeper Tree " $ do
    map (*4) (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` (Branch (Branch (Leaf 4) (Leaf 8)) (Branch (Leaf 16) (Leaf 20)))
   it "works for unbalanced trees " $ do
    map (+4) (Branch (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) (Leaf 4)) `shouldBe` (Branch (Branch (Branch (Leaf 4) (Leaf 5)) (Leaf 6)) (Leaf 8))


  describe "size2" $ do
   it "works for Tree with single leaf" $ do
    size2 (Leaf 1) `shouldBe` 1
   it "works for Tree with branch having two leaves" $ do
    size2 (Branch (Leaf 1) (Leaf 2)) `shouldBe` 3
   it "works for deeper Tree " $ do
    size2 (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 7
   it "works for unbalanced trees " $ do
    size2 (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 4)) `shouldBe` 5

  describe "maximum2" $ do
   it "works for Tree with single leaf" $ do
    maximum2 (Leaf 5) `shouldBe` 5
   it "works for Tree with branch having just two leaves" $ do
    maximum2 (Branch (Leaf 1) (Leaf 2)) `shouldBe` 2
   it "works for deeper Tree " $ do
    maximum2 (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 5
   it "works for unbalanced trees " $ do
    maximum2 (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 4)) `shouldBe` 4


  describe "depth2" $ do
   it "works for Tree with single leaf" $ do
    depth2 (Leaf 5) `shouldBe` 1
   it "works for Tree with branch having just two leaves" $ do
    depth2 (Branch (Leaf 1) (Leaf 2)) `shouldBe` 2
   it "works for deeper Tree " $ do
    depth2 (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 4) (Leaf 5))) `shouldBe` 3
   it "works for unbalanced trees " $ do
    depth2 (Branch (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) (Leaf 4)) `shouldBe` 4

  describe "map2" $ do
   it "works for Tree with single leaf" $ do
    map2 (\x -> Leaf (x + 2) ) (\l r -> Branch l r) (Leaf 5) `shouldBe` (Leaf 7)
   it "works for Tree with branch having just two leaves" $ do
    map2 (\x -> Leaf (x * 3) ) (\l r -> Branch l r) (Branch (Leaf 1) (Leaf 2)) `shouldBe` (Branch (Leaf 3) (Leaf 6))

