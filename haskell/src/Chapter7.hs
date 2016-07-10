module Chapter7 where
import Prelude hiding (sequence)
-- 7.2
data ExecutorService = ExecutorService deriving (Show, Eq)
data Future a = Future a deriving (Show, Eq)
type Par a = ExecutorService -> Future a

unit :: a -> Par a
unit a = \es -> Future a

get :: Future a -> a
get (Future a) = a

run :: Par a -> ExecutorService -> a
run pa es =  get (pa es)

fork :: Par a -> Par a
fork p = \es -> Future $ get (p es)         -- p will be lazily evaluated

lazyUnit :: a -> Par a
lazyUnit = fork . unit

-- 7.1
map2 :: (a -> b -> c) -> Par a -> Par b ->  Par c
map2 f e g = \es -> Future $ f  (get (e es)) (get(g es))

-- 7.4
asyncF :: (a -> b) -> ( a ->  Par b)
asyncF f = \x -> unit $ f x


_map :: (a -> b) -> Par a -> Par b
_map f pa = map2 (\x _ -> f x )  pa (unit () )

sums :: (Num a) => [a] -> Par a
sums [] = unit 0
sums [x] = unit x
sums xs = map2 (+) left right
          where l = length xs  `div` 2
                left =  fork (sums (take l xs) ) -- Par (Par a)
                right = fork (sums (drop l xs) ) -- Par (Par b)

--7.5
sequence :: [Par a] ->  Par[a]
sequence = foldr (map2 (:)) (unit([]))

--7.6
parFilter :: (a -> Bool) -> [a] -> Par [a]
parFilter f xs = map2 (\a _ -> concatMap (id) a )  ts  (unit [])
                where  ts =  sequence( map (\x -> unit( if (f x) then [x] else [] ) ) xs)

map3 :: (a -> b -> c -> d) -> Par a -> Par b -> Par c -> Par d
map3 f a b c =  map2 (\g z -> g z )  (map2 (\x y -> f x y  ) a b) c

map4 :: (a -> b -> c -> d -> e) -> Par a -> Par b -> Par c -> Par d -> Par e
map4 f a b c d =  map2 (\g t -> g t ) (map3 (\x y z -> f x y z  ) a b c) d

-- 7.11
choiceN :: Par Int -> [Par a] -> Par a
choiceN n choices = \es -> (choices !! (get (n es)))es

choice :: Par Bool -> Par a -> Par a -> Par a
choice pb pt pf =  choiceN (_map (\b -> if( b ) then 0 else 1) pb) [pt,pf]

-- 7.13
chooser :: Par a -> (a -> Par b) -> Par b
chooser pa f = \es -> f (get (pa es)) es

-- 7.14
_join :: Par (Par a) -> Par a
_join ppa = \es -> (get (ppa es)) es