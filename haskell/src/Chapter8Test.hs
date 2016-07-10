import Test.Hspec
import Test.QuickCheck
import Data.List
import Chapter8
import Chapter6

main = hspec $
 describe "Chapter8" $ do
  -- 8.4
  describe "choose" $ do -- 8.4
   it "generates number in a given range" $ do
     next (Chapter8.choose 1 10) (SimpleGenerator 2) `shouldBe` (1,SimpleGenerator 3185167589)     

 -- 8.5

  describe "unit" $ do -- 8.4
   it "generates same number for any RNG" $ do
     next (Chapter8.unit 1) (SimpleGenerator 2) `shouldBe` (1,SimpleGenerator 2)
     next (Chapter8.unit 1) (SimpleGenerator 10) `shouldBe` (1,SimpleGenerator 10)


  describe "boolean" $ do 
   it "generates boolean for RNG" $ do
     next (Chapter8.boolean) (SimpleGenerator 1) `shouldBe` (True,SimpleGenerator 3740067448)
     next (Chapter8.boolean) (SimpleGenerator 3740067448) `shouldBe` (False,SimpleGenerator 1217261859)


  describe "pair" $ do 
   it "generates pair for RNG" $ do
     next (Chapter8.pair 1 10) (SimpleGenerator 1) `shouldBe` ((8,3),SimpleGenerator 1217261859)
     next (Chapter8.pair 1 1) (SimpleGenerator 1) `shouldBe` ((1,1),SimpleGenerator 1217261859)
     next (Chapter8.pair (-100) 100) (SimpleGenerator 1) `shouldBe` ((68,73),SimpleGenerator 1217261859)
           

  describe "flatMap" $ do 
   it "should work for  flatMap" $ do
     next (Chapter8.flatMap (\x -> Chapter8.unit x) (Chapter8.choose 1 10)) (SimpleGenerator 1) `shouldBe` (8,SimpleGenerator 3740067448)
     
  
  describe "union" $ do 
   it "should work for union" $ do
     next (Chapter8.union (Chapter8.unit 1) (Chapter8.unit 2)) (SimpleGenerator 1) `shouldBe` (2,SimpleGenerator 3740067448)
     next (Chapter8.union (Chapter8.unit 1) (Chapter8.unit 2)) (SimpleGenerator 3740067448) `shouldBe` (1,SimpleGenerator 1217261859)

  describe "weighted" $ do 
   it "should work for weighted" $ do
     next (Chapter8.weighted (Chapter8.unit 1, 0.75) (Chapter8.unit 2, 0.25)) (SimpleGenerator 1) `shouldBe` (2,SimpleGenerator 3740067448)
     next (Chapter8.weighted (Chapter8.unit 1, 0.75) (Chapter8.unit 2, 0.25)) (SimpleGenerator 3740067448) `shouldBe` (1,SimpleGenerator 1217261859)
     next (Chapter8.weighted (Chapter8.unit 1, 0.75) (Chapter8.unit 2, 0.25)) (SimpleGenerator 1217261859) `shouldBe` (1,SimpleGenerator 291053042)
     next (Chapter8.weighted (Chapter8.unit 1, 0.75) (Chapter8.unit 2, 0.25)) (SimpleGenerator 291053042) `shouldBe` (1,SimpleGenerator 3941674005)


  describe "forAll" $ do 
   it "should work" $ do
     run (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x > 0 Prelude.&& x < 11)) 100 (SimpleGenerator 1) `shouldBe` (Passed)
     run (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x < 9 )) 100 (SimpleGenerator 1) `shouldBe` (Falsified "()" 22)


  describe "&&" $ do 
   it "should work for two properties" $ do
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x > 0)) Chapter8.&& (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <= 9 ))) 100 (SimpleGenerator 1) `shouldBe` (Passed)
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x > 0)) Chapter8.&& (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <  9 ))) 100 (SimpleGenerator 1) `shouldBe` (Falsified "second" 1)
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x < 0)) Chapter8.&& (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <  9 ))) 100 (SimpleGenerator 1) `shouldBe` (Falsified "first" 0)
          

  describe "||" $ do 
   it "should work for two properties" $ do
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x > 0)) Chapter8.|| (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <= 9 ))) 100 (SimpleGenerator 1) `shouldBe` (Passed)
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x > 0)) Chapter8.|| (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <  9 ))) 100 (SimpleGenerator 1) `shouldBe` (Passed)
     run ((Chapter8.forAll (Chapter8.choose 1 10) (\x -> x < 0)) Chapter8.|| (Chapter8.forAll (Chapter8.choose 1 10) (\x -> x <  9 ))) 100 (SimpleGenerator 1) `shouldBe` (Falsified "second" 0)

--listOf :: (RNG r) => Gen a r -> SGen [a] r
  describe "listOf" $ do 
   it "should generate list of desired size" $ do
     nextList (Chapter8.listOf (Chapter8.choose 1 10)) 10 (SimpleGenerator 1) `shouldBe` ([8,3,1,5,4,5,1,4,7,1],SimpleGenerator 1589295307)


--listOf1
  describe "listOf1" $ do 
   it "should never generate empty list" $ do
    run ( Chapter8.forAll ((Chapter8.listOf1 (Chapter8.choose 1 10) (Chapter8.choose 1 100) )) (\xs -> (length xs) >= 1) ) 100 (SimpleGenerator 1) `shouldBe` (Passed)


next :: (RNG r) => Chapter8.Gen a r -> r -> (a, r)
next (Chapter8.Gen s) rng = runState s rng 

nextList :: (RNG r) => Chapter8.SGen [a] r -> Int -> r -> ([a], r)
nextList (SGen s) n r = next (s n) r
                       
