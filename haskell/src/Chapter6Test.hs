import Test.Hspec
import Test.QuickCheck
import Data.List
import Chapter6

main = hspec $
 describe "Chapter6" $ do
  describe "nonNegativeInt" $ do
   it "generates non negative int for positive seed" $ do
     nonNegativeInt (SimpleGenerator 2) `shouldBe` (48601,SimpleGenerator 3185167589)
   it "generates non negative int for negative seed" $ do
     nonNegativeInt (SimpleGenerator (-2)) `shouldBe` (16934,SimpleGenerator 1109799729)
   it "generates non negative int for a large range of numbers" $ do
     filter (<0) (map (\s -> fst (nonNegativeInt (SimpleGenerator 2) )) [-100,-99..100]) `shouldBe` ([])

  describe "double" $ do
   it "generates same double value for a given seed" $ do
     double (SimpleGenerator 2) `shouldBe` (5.2693309784968e-15,SimpleGenerator 3185167589)

  describe "intDouble" $ do
   it "generates same int and double value for a given seed" $ do
     intDouble (SimpleGenerator 2) `shouldBe` ((48601,7.020100646626393e-15),SimpleGenerator 4243454860)

  describe "doubleInt" $ do
   it "generates same double and int value for a given seed" $ do
     doubleInt (SimpleGenerator 2) `shouldBe` ((7.020100646626393e-15,48601),SimpleGenerator 4243454860)

  describe "doubleInt" $ do
   it "generates three different doubles a given seed" $ do
     double3 (SimpleGenerator 2) `shouldBe` ((5.2693309784968e-15,7.020100646626393e-15,5.728382178327163e-15),SimpleGenerator 3462652583)

-- 6.4
  describe "ints" $ do
   it "generates empty list for n = 0" $ do
     ints 0 (SimpleGenerator 3) `shouldBe` ([],SimpleGenerator 3)
   it "generates a list random ints for n > 0" $ do
     ints 10 (SimpleGenerator 3) `shouldBe` ([40134,45390,35694,55625,35226,5957,19977,8715,48241,40133],SimpleGenerator 2630211037)
   it "generated list contains n random elements " $ do
    length( fst ( ints 10 (SimpleGenerator 3) ) ) `shouldBe` (10)
   it "generated list contains n distinct random elements " $ do
    nub ( fst ( ints 10 (SimpleGenerator 3) ) ) `shouldBe` ([40134,45390,35694,55625,35226,5957,19977,8715,48241,40133])


-- 6.5
  describe "_map" $ do
   it "map (Rand a r) -> (show) -> (Rand b r) generates a random string" $ do
    fst( _map nonNegativeInt show (SimpleGenerator 2)) `shouldBe` ("48601")
 
 -- 
 
  describe "map2" $ do
   it "map2 generates a random number by combining two rands " $ do
     fst ((map2 (+) nonNegativeInt nonNegativeInt) (SimpleGenerator 2))  `shouldBe` (113350)


-- 6.6
  describe "both" $ do
   it "both should combine results of two rands " $ do
     fst((both nonNegativeInt (ints 2)) (SimpleGenerator 2))  `shouldBe` (48601,[64749,52835])

-- 6.7 sequence    
  describe "sequence" $ do
   it "should work for sequences of rand " $ do
     fst ( Chapter6.sequence (replicate 5 nonNegativeInt) (SimpleGenerator 2))  `shouldBe`  ([48601,64749,52835,25117,36000])

-- 6.10
  describe "map2S" $ do
   it "should combine two states" $ do
     runState (map2S (+) (unit 3) (unit 4)) "start"  `shouldBe` (7,"start")

-- 6.11
  describe "simulateMachine" $ do
   it "gives no candy for no input " $ do
     runState (simulateMachine []) (Machine True (3,4)) `shouldBe` (((3,4),Machine True (3,4))::((Int, Int), Machine))
   it "gives no candy when it is empty" $ do
     runState (simulateMachine [Turn]) (Machine False (0,4)) `shouldBe` ((0,4), Machine False (0,4))
   it "gives no candy when it is locked" $ do
     runState (simulateMachine [Turn]) (Machine True (4,0)) `shouldBe` ((4,0), Machine True (4,0))
   it "gives candy when coin is inserted and turned" $ do
     runState (simulateMachine [Coin, Turn]) (Machine True (1,0)) `shouldBe` ((0,1), Machine True (0,1))
   it "gives 2 candies when 2 coins are inserted and turned and machine has enough candies" $ do
     runState (simulateMachine [Coin, Turn, Coin, Turn]) (Machine True (5,5)) `shouldBe` ((3,7), Machine True (3,7))
   it "machine is locked after dispensing candy" $ do
     runState (simulateMachine [Turn]) (Machine False (5,5)) `shouldBe` ((4,5), Machine True (4,5))
 

