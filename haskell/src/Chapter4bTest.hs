import Test.Hspec
import Test.QuickCheck
import Prelude hiding(map, filter, sequence, Either(Left), Either(Right), traverse)
import Data.List (foldl')
import Chapter4b (Either(Left), Either(Right), map, flatMap, orElse, sequence,
                        map2, traverse, sequence22)

main = hspec $
 describe "Chapter4b" $ do

  describe "map" $ do
   it "works for Left" $ do
    map (+1) (Left "hello") `shouldBe` (Left "hello" :: Either String Int)
   it "should work for map (+1) Right(2) => Right(3) " $ do
    map (+1) (Right 2) `shouldBe` (Right 3::Either String Int)

  describe "flatMap" $ do
   it "works for Left" $ do
    flatMap (\x -> if (x > 0) then Right x else Left "fail" ) (Left "failed") `shouldBe` (Left "failed" :: Either String Int)
   it "should work for flatMap (x -> Right(x + 3) ) Right(2) => Right(5)" $ do
    flatMap (\x -> Right(x + 3) ) (Right 2) `shouldBe` (Right 5 :: Either String Int)

  describe "orElse" $ do
   it "works for when first and second arguments are Left" $ do
    orElse (Left "1") (Left "2") `shouldBe` (Left "2":: Either String Int)
   it "works for when first argument is Left and second is Right" $ do
    orElse (Left "1") (Right 1) `shouldBe` (Right 1:: Either String Int)
   it "works for when first and second arguments are Right" $ do
    orElse (Right 1) (Right 2) `shouldBe` (Right 1:: Either String Int)
   it "works for when first argument is Right and second is Left" $ do
    orElse (Right 1) (Left "3") `shouldBe` (Right 1:: Either String Int)

  describe "map2" $ do
   it "works for when first and second arguments are Left" $ do
    map2 (+) (Left "1") (Left "2") `shouldBe` (Left "1":: Either String Int)
   it "works for when first argument is Left and second is Right" $ do
    map2 (+) (Left "1") (Right 1) `shouldBe` (Left "1":: Either String Int)
   it "works for when first and second arguments are Right" $ do
    map2 (+)  (Right 1) (Right 2) `shouldBe` (Right 3:: Either String Int)
   it "works for when first argument is Right and second is Left" $ do
    map2 (+)  (Right 1) (Left "3") `shouldBe` (Left "3":: Either String Int)

  describe "sequence" $ do
   it "works when list is empty" $ do
    sequence [] `shouldBe` (Right []:: Either String [Int])
   it "works when list has only Lefts" $ do
    sequence [Left "1", Left "2"] `shouldBe` (Left "1":: Either String [Int])
   it "works when lists has Lefts and Rights" $ do
    sequence [Left "1", Right 1] `shouldBe` (Left "1":: Either String [Int])
   it "works when list has Rights" $ do
    sequence [Right 1, Right 2] `shouldBe` (Right [1,2]:: Either String [Int])

--traverse :: (a -> Either e b) -> [a] -> Either e [b]
  describe "traverse" $ do
   it "works when list is empty" $ do
    traverse (\x -> Right x) [] `shouldBe` (Right []:: Either String [Int])
   it "works when list has only Lefts" $ do
    traverse (\x -> Left (show x) ) [ 1,  2] `shouldBe` (Left "1":: Either String [Int])
   it "works when lists has Lefts and Rights" $ do
    traverse (\x -> if (even x) then Right x else Left (show x)) [1,2] `shouldBe` (Left "1":: Either String [Int])
   it "works when list has Rights" $ do
    traverse (\x -> Right x) [1,2] `shouldBe` (Right [1,2]:: Either String [Int])


  describe "sequence22" $ do
   it "works when list is empty" $ do
    sequence22 [] `shouldBe` (Right []:: Either [String] [Int])
   it "works when list has only Lefts" $ do
    sequence22 [Left "1", Left "2"] `shouldBe` (Left ["1","2"]:: Either [String] [Int])
   it "works when lists has Lefts and Rights" $ do
    sequence22 [Left "1", Right 1] `shouldBe` (Left ["1"]:: Either [String] [Int])
   it "works when list has Rights" $ do
    sequence22 [Right 1, Right 2] `shouldBe` (Right [1,2]:: Either [String] [Int])
