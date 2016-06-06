import Test.Hspec
import Test.QuickCheck
import Chapter5 (forAll, unfold, zipAll, startsWith, tails, scanRight)

main = hspec $
 describe "Chapter5" $ do
  describe "forAll" $ do
   it "works for empty List" $ do
     Chapter5.forAll (>0) [] `shouldBe` (True)
   it "should work for list when all elements satisfy the predicate " $ do
     Chapter5.forAll (>0) [2,3,4] `shouldBe` (True)
   it "should work for list when some elements satisfy the predicate " $ do
     Chapter5.forAll (>0) [2,3,4,0] `shouldBe` (False)
   it "should work for infinite list when some elements do not satisfy the predicate " $ do
     Chapter5.forAll (>0) (2:3:4:[0..]) `shouldBe` (False)

  describe "unfold" $ do
   it "works generates empty list when there are no state transitions." $ do
     unfold (\x -> Nothing) (0)  `shouldBe` ([]::(Num a) => [a])
   it "generates a non-empty list for the given function" $ do
     unfold (\x -> if( x < 4 ) then Just(x , x + 1 ) else Nothing) (0)  `shouldBe` ([0,1,2,3])
   it "can map list for the given function" $ do
     unfold (\x -> if( not( null x) ) then Just( (head x) ^ 2 , tail x ) else Nothing) ([2,3,4])  `shouldBe` ([4,9,16])
   it "can take from list using unfold" $ do
     unfold (\(n , xs) -> if( n == 0 || null xs) then Nothing else Just((head xs)  , ( n - 1 , tail xs)) ) (3,[2,3,4,5,6])  `shouldBe` ([2,3,4])

  describe "zipAll" $ do
   it "works when both lists are empty." $ do
     zipAll ([]::(Num a) => [a]) ([]::(Num a) => [a]) `shouldBe` []
   it "works when first list is empty." $ do
     zipAll ([]::(Num a) => [a]) ([2]) `shouldBe` [(Nothing, Just 2)]
   it "works when second list is empty." $ do
     zipAll ([2]) ([]::(Num a) => [a]) `shouldBe` [(Just 2, Nothing)]
   it "works when lists are not empty." $ do
     zipAll ([2]) ([3]) `shouldBe` [(Just 2, Just 3)]
   it "works when first list is bigger than second." $ do
     zipAll ([2, 4]) ([3]) `shouldBe` [(Just 2, Just 3), (Just 4, Nothing)]
   it "works when second list is bigger than first." $ do
     zipAll ([2]) ([3,4]) `shouldBe` [(Just 2, Just 3), (Nothing,Just 4)]


  describe "startsWith" $ do
   it "works when both lists are empty." $ do
     startsWith ([]::(Num a) => [a]) ([]::(Num a) => [a]) `shouldBe` True
   it "works when first list is empty." $ do
     startsWith ([]::(Num a) => [a]) ([2]) `shouldBe` False
   it "works when second list is empty." $ do
     startsWith ([2]) ([]::(Num a) => [a]) `shouldBe` True
   it "works when fisrt list contains second." $ do
     startsWith ([2, 3, 4]) ([2, 3]) `shouldBe` True
   it "works when fisrt list does not contain second." $ do
     startsWith ([2, 3, 4]) ([4, 3]) `shouldBe` False

  describe "tails" $ do
   it "works with empty list." $ do
     tails ([]::(Num a) => [a]) `shouldBe` []
   it "works with non empty list." $ do
     tails [2,3,4] `shouldBe` [[2,3,4],[3,4],[4]]

  describe "scanRight" $ do
   it "works with empty list." $ do
     scanRight (+) (0) ([]::(Num a) => [a]) `shouldBe` []
   it "works with non empty list." $ do
     scanRight (+) (0) ([2,3,4]) `shouldBe` [9,7,4]
   it "can generate tails." $ do
     scanRight (:) ([]) ([2,3,4]) `shouldBe` [[2,3,4] , [3,4] , [4]]
