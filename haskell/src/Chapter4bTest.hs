import Test.Hspec
import Test.QuickCheck
import Prelude hiding(map, filter, sequence, Either(Left), Either(Right))
import Data.List (foldl')
import Chapter4b (Either(Left), Either(Right), map, flatMap, orElse)
                    --, flatMap, getOrElse, orElse)

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

