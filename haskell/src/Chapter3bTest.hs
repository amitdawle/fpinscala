import Test.Hspec
import Test.QuickCheck

import Prelude hiding (maximum, map)

import Chapter3b (Tree(Leaf), Tree(Branch), size, maximum, depth, map)

main = hspec $
 describe "Chapter3b" $ do

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

