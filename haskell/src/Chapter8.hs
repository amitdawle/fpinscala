module Chapter8 where
import Chapter6
import qualified Chapter7 as C7

import Data.List

--class Gen g where
--  gen :: g -> ([Int], g)

type SuccessCount = Int
type FailedCase = String
type TestCases = Int
--type Result = Maybe (FailedCase, SuccessCount)


data Gen a r = Gen (State r a)
-- 8.4
choose :: (RNG r) => Int -> Int -> Gen Int r
choose s e = Gen ( State (\rng -> let (v, r1) = nonNegativeInt rng
                                  in (Chapter8.adjust s e v, r1 )))

adjust s e n = if  n > e then Chapter8.adjust s e (snd (n `divMod` e)) else if n < s then s else n

-- 8.5
unit :: (RNG r) => a -> Gen a r
unit a = Gen ( State (\r ->(a,r)))

boolean :: (RNG r) => Gen Bool r
boolean  = Gen (State $(\r -> (bool r) )  )
           where bool r = let (v, r2) =  nonNegativeInt r
                              in (if even v then True else False, r2)

listOfN :: (RNG r) => Int -> Gen a r -> Gen [a] r
listOfN 0 _  = Gen (State (\r -> ([] , r)) )
listOfN n (Gen s1) = Gen ( State (\r -> let (x, r1) = runState s1 r
                                            (Gen s2) = listOfN (n - 1) (Gen s1)
                                            (xs, r2) =  runState s2 r1
                                        in  (x:xs, r2)))

pair :: (RNG r) => Int -> Int -> Gen (Int, Int) r
pair start end  = Gen (State (\r -> let (Gen s) = choose start end
                                        (x1, r1) = runState s r
                                        (x2, r2) = runState s r1
                                    in  ((x1, x2), r2)))

toOption :: (RNG r) => Gen a r -> Gen (Maybe a) r
toOption (Gen s) = Gen (State (\r -> let (x, r1) = runState s r
                                         in (Just x, r1)))

fromOption :: (RNG r) => Gen (Maybe a) r -> Gen a r
fromOption (Gen s) = Gen (State (\r -> case (runState s r) of
                                      (Just x, r1) -> (x , r1)
                                      (Nothing, _)  -> error "empty" ))


fromOptionOrElse :: (RNG r) => Gen (Maybe a) r -> a -> Gen a r
fromOptionOrElse (Gen s) d = Gen (State (\r -> case (runState s r) of
                                      (Just x, r1) -> (x , r1)
                                      (Nothing, _) -> (d, r) ))

mapGen :: (RNG r) => (a -> b) -> Gen a r -> Gen b r
mapGen  f (Gen s) = Gen (State (\r -> let (x, r1) = runState s r
                                      in (f x , r1)))
-- 8.6

flatMap :: (RNG r) => (a -> Gen b r) -> Gen a r -> Gen b r
flatMap f (Gen s) = Gen (State (\r -> let (a, r1) = runState s r
                                          (Gen s2) = f a
                                          (b, r2) = runState s2 r1
                                      in  (b, r2)
                                    ))

listOfN2 :: (RNG r) => Gen Int r -> Gen a r -> Gen [a] r
listOfN2 (Gen n) (Gen s1) = Gen ( State (\r -> let (n1, r1) = runState n r
                                                   (Gen s2) = if  n1 < 0  then listOfN 0 (Gen s1) else  listOfN n1 (Gen s1)
                                                   (xs, r2) =  runState s2 r1
                                               in  (xs , r2) ))

-- 8.7
union :: (RNG r) => Gen a r -> Gen a r -> Gen a r
union (Gen s1) (Gen s2) = Gen (State (\r -> let (x, r1) = (nonNegativeInt r)
                                                (v, r2) = if (odd x) then runState s1 r1 else runState s2 r1
                                            in  (v, r2)
                           ))

-- 8.8
weighted :: (RNG r) => (Gen a r, Double) -> (Gen a r, Double) -> Gen a r
weighted (Gen s1, d1) (Gen s2, d2) = Gen (State(\r ->  let Gen g = (choose 1 10) 
                                                           (a, r1) = runState g r
                                                           w = fromIntegral (a) / 10.0
                                                           wMax = max d1 d2
                                                           (v, r2) = if( w <= wMax ) then runState s1 r1 else runState s2 r1
                                                       in  (v, r2)))

-- 8.9
data Result = Passed | Falsified FailedCase SuccessCount | Proved deriving (Eq, Show)
newtype Prop n r = Prop { run ::  n -> r -> Result }

forAll :: (RNG r) => Gen a r -> (a -> Bool) -> Prop Int r
forAll g f  = Prop (\n r -> let xs = zip (take n (unfold r g)) [1..]
                                ys = map (\(x, i) -> if f x then Passed else Falsified "()" i ) xs
                                res = filter (\x -> x /= Passed) ys
                               in  (if null res then Passed else head res) )

-- 8.9
(&&) :: Prop n r -> Prop n r -> Prop n r
(&&) p q = Prop(\n r -> if ( run p n r == Passed Prelude.|| run p n r == Proved ) then case (run q n r) of
                                                                                Passed -> Passed
                                                                                Proved -> Proved
                                                                                _ -> Falsified "second" 1
                        else Falsified "first" 0)

(||) :: Prop n r -> Prop n r -> Prop n r
(||) p q = Prop(\n r -> if ( run p n r == Passed ) then Passed
                        else  case (run q n r) of
                                    Passed -> Passed
                                    _ -> Falsified "second" 0)

--take 10 ( unfold  (SimpleGenerator 100)  (choose 1 10) )
unfold :: (RNG r) => r -> Gen a r -> [a]
unfold r (Gen s) = let (a, r1) = runState s r
                   in a : (unfold  r1 (Gen s))

nextInt :: (RNG r) => Gen Int r -> r -> Int
nextInt (Gen s) r  =  fst (runState s r)

nextBool :: (RNG r) => Gen Bool r -> r -> Bool
nextBool (Gen s) r  =  fst (runState s r)

------------------
newtype SGen a r = SGen (Int -> Gen a r)

-- 8.10
unsized :: (RNG r) => Gen a r -> SGen a r
unsized (Gen s) = SGen(\a -> Gen s)

-- 8.11
unitSGen :: (RNG r) => a -> SGen a r
unitSGen a = unsized (Chapter8.unit a)

booleanSGen :: (RNG r) => SGen Bool r
booleanSGen = unsized (boolean)

-- 8.12
listOf :: (RNG r) => Gen a r -> SGen [a] r
listOf g = SGen(\n -> listOfN n g)

----
runProp ::(RNG r, Show a) => Prop a r -> a -> r -> IO ()
runProp p n rng = case (run p n rng) of
                                  Proved -> putStrLn $ "Proved "
                                  Passed -> putStrLn $ "Passed " ++ (show n)
                                  (Falsified f s) -> putStrLn $ "Failed " ++ f

-- 8.13
listOf1 :: (RNG r) => Gen Int r -> Gen a r -> Gen [a] r
listOf1 (Gen sn) g2 = listOfN2 g1 g2
                  where g1 = Gen (State(\r -> let (n, _) = runState sn r
                                             in (if n <= 0 then 1 else n, r )))

-- 8.14
propSort :: RNG r => Prop Int r
propSort  = forAll ( listOf1 (choose 0 100) (choose (-10) 10) ) (\xs -> if length xs == 1 then True else let sxs = sort xs in all (\(x, y) -> x <= y  )  (zip sxs (tail sxs)))

check :: (RNG r) => Prop Bool r
check = Prop(\p _ -> if(p) then Proved else Falsified "()" 0 )

--weighted :: (RNG r) => (Gen a r, Double) -> (Gen a r, Double) -> Gen a r
s :: (RNG r) =>  Gen C7.ExecutorService r
s = weighted (mapGen (\_ -> C7.ExecutorService) (choose 1 4), 0.75) (Chapter8.unit(C7.ExecutorService), 0.25)

(**) :: Gen a r -> Gen b r -> Gen (a, b) r
(**) (Gen s1) (Gen s2) = Gen(State(\rng -> runState (map2S (\a b -> (a,b)) s1 s2) rng))

-- forAll :: (RNG r, Show a) => Gen a r -> (a -> Bool) -> Prop Int r
-- forAllPar :: (RNG r) => (f -> Par a) ->  Gen a r -> Prop ??? ???
forAllPar :: (RNG r) => (a -> C7.Par Bool) -> Gen a r -> Prop Int r
forAllPar f g =  forAll (s Chapter8.** g)  (\(e, v) -> C7.get ( (f v) e) )


equal :: (Eq a) => C7.Par a -> C7.Par a -> C7.Par Bool
equal = C7.map2 (\a b -> a == b)

-- let gp =  mapGen (\x -> Chapter7.unit x) (choose 1 10)
-- forAllPar (\p ->  equal ( Chapter7._map (id) p ) p) gp
-- runProp (forAllPar (\p ->  equal ( Chapter7._map (id) p ) p) gp ) 100 (SimpleGenerator 100)
--   Passed 100
---8.17
--let gp =  mapGen (\x -> Chapter7.fork $ Chapter7.unit x) (choose 1 10)
--runProp (forAllPar (\p ->  equal ( Chapter7._map (id) p ) p) gp ) 100 (SimpleGenerator 100)

-- 8.18
propTakeWhile :: (RNG r) => ( Int -> Bool ) -> Prop Int r
propTakeWhile f = (forAll g1 (\xs -> let xs1 = takeWhile (f) xs in ( xs1 == (filter (f) xs1))))
                  where g1 =  listOf1 (choose 1 100) (choose 1 100)