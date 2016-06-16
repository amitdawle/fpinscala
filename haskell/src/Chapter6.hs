module Chapter6 where

import Control.Monad (liftM, ap)
import Prelude hiding (sequence)

import Data.Bits

class RNG g  where
   nextInt :: g -> (Int, g)

---- Simple Generator ------------------------
data SimpleGenerator = SimpleGenerator Int deriving (Show, Eq)

instance RNG SimpleGenerator where
   nextInt = simpleNextInt

simpleNextInt :: SimpleGenerator  -> (Int, SimpleGenerator)
simpleNextInt (SimpleGenerator seed) = (shift newSeed (-16) , SimpleGenerator newSeed)
                                     where newSeed = (.&.) (seed * 0x5DEECE66D + 0xB ) 0xFFFFFFFF

----------------------------------------

-- Exercise 6.1
nonNegativeInt :: (RNG r) => r -> (Int, r)
nonNegativeInt = \rng -> let (i, rng2) = nextInt rng
                         in (fabs i  , rng2 )
                         where fabs x = if x == (minBound::Int)  then 0 else abs x 

-- Exercise 6.2
double :: (RNG r) => r ->(Double , r)
double rng =  let (i , rng2) = nonNegativeInt rng
              in ( adjust i , rng2 )

adjust :: (Fractional b) => Int -> b
adjust x = if  x == (maxBound::Int)  then 0.0 else fromIntegral x/fromIntegral (maxBound::Int)

-- Exercise 6.3
intDouble :: (RNG r) => r -> ((Int, Double) , r)
intDouble rng =  let (i , r2 ) = nextInt rng
                     (d , r3 ) = double r2
                     in ((i , d) , r3)

doubleInt :: (RNG r) => r -> ((Double, Int) , r)
doubleInt rng =  let ((i, d), r2) = intDouble rng
                 in ((d, i), r2)

double3 :: (RNG r) => r -> ((Double, Double,Double) , r)
double3 rng =  let (d1, r2 ) = double rng
                   (d2, r3 ) = double r2
                   (d3, r4 ) = double r3
                in ((d1, d2, d3) , r4)

-- Exercise 6.4
ints :: (RNG r) => Int -> r -> ([Int] , r )
ints 0 rng  = ([], rng)
ints n rng = (i : l , r3)
           where (i, r2) = nextInt rng
                 (l, r3) = ints (n - 1) r2



type Rand a g = g -> (a , g)

_map :: (RNG r) =>  Rand a r  -> (a -> b) -> Rand b r
_map r f rng =  let (a , r2) = r rng
                in (f a  , r2)


-- Exercise 6.5

double2 :: (RNG r) => (Rand Double r)
double2 = _map  nonNegativeInt adjust 
        

-- Exercise 6.6
map2 ::(RNG r) => (a -> b -> c) -> Rand a r ->  Rand b r -> Rand c r
map2 f g h rng =  let (a , r1) = g rng
                      (b , r2 ) = h r1
                  in  (f a b , r2)

both ::(RNG r) =>  Rand a r -> Rand b r -> Rand (a,b) r
both = map2 (\x y -> (x,y))  


-- Exercise 6.7
sequence :: (RNG r) => [Rand a r] -> Rand [a] r
sequence [] = \rng -> ([], rng)
sequence (x:xs) = \rng -> let (a, g) = x rng
                              (l, s) = sequence xs g
                          in (a : l, s )

-- Exercise 6.10
newtype State s a = State { runState :: s -> (a, s)} 

get :: State s s
get = State(\s -> (s, s))

set :: s -> State s ()
set s = State(\_ -> ((),  s) )

unit :: a -> State s a 
unit x = State $ \s -> (x, s)


instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where 
    pure = return
    (<*>) = ap 

instance Monad (State s) where
    return x = State(\s ->(x, s))
    m >>= k = State $ \s -> let (a, s1) = runState m s
                  in runState  (k a ) s1

map2S :: (a->b->c) -> State s a -> State s b -> State s c
map2S f sa sb = State $ \s -> let (a, s1) = runState sa s
                                  (b, s2) = runState sb s1
                            in (f a b, s2)  


sequenceS :: [State s a] -> State s [a]
sequenceS = foldr (map2S (:) ) (unit []) 


data Input = Coin | Turn  deriving (Show, Eq)
data Machine = Machine Bool (Int, Int) deriving (Show, Eq)

modify :: (s->s) -> State s ()
modify f = do c <- get
              set $ f c 


-- Exercise 6.11

simulateMachine :: [Input] -> State Machine (Int, Int)
simulateMachine inputs = do _ <- sequenceS ( map (\i -> modify (\m -> transition i m) ) inputs )   
                            (Machine _ (candy, coin) ) <- get
                            return (candy, coin)
                            
                         

transition :: Input -> Machine -> Machine
transition _  m@(Machine _ (0 ,_)) = m
transition Coin (Machine False (candy, coin)) = Machine False  (candy, (coin + 1))
transition Coin (Machine True (candy, coin)) = Machine False (candy, (coin + 1))
transition Turn  m@(Machine True (_, _)) = m
transition Turn (Machine False (candy, coin)) = Machine True ((candy - 1), (coin))



type Stack = [Int]  

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push:: Int -> State Stack ()
push x = State $ \xs -> (() , x:xs) 

