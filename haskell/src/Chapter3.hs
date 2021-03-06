module Chapter3 where
import Prelude hiding (tail,
                       dropWhile,
                       takeWhile,
                       drop,
                       init,
                       length,
                       reverse,
                       foldl,
                       concat,
                       map,
                       filter, zipWith)


data List a = Empty | Cons a (List a) deriving (Eq, Show)

-- Exercise 3.2
tail :: (List a) -> (List a)
tail Empty = Empty
tail (Cons _ t) = t

-- Exercise 3.3
setHead :: a -> (List a) -> List a
setHead s Empty = Cons s Empty
setHead s (Cons h t) = Cons s t

-- Exercise 3.4
drop :: Int -> (List a) -> (List a)
drop _ Empty = Empty
drop n (Cons h t)
     | n <= 0 = (Cons h t)
     | otherwise = drop (n - 1) t

-- tail as drop
tail2 :: (List a) -> (List a)
tail2  = drop 1

-- Exercise 3.5
dropWhile :: (a -> Bool) -> (List a) -> (List a)
dropWhile _ Empty = Empty
dropWhile f (Cons h t) = if( f h ) then dropWhile f t else (Cons h t)


-- Exercise 3.6
init ::  (List a) -> (List a)
init Empty = Empty
init (Cons x Empty) = Empty
init (Cons h t) = (Cons h (init t))


takeWhile :: (a -> Bool) -> (List a) -> (List a)
takeWhile _ Empty = Empty
takeWhile f (Cons h t) = if (f h) then (Cons h (takeWhile f t)) else Empty

-- Exercise 3.7
foldRight :: ( a -> b -> b ) -> b -> (List a) -> b
foldRight _ b Empty = b
foldRight f b (Cons h t) = f h (foldRight f b t)

-- Exercise 3.8 is already covered by 3.7

-- Exercise 3.9
length :: (List a) -> Int
length xs = foldRight (\x t -> t + 1) 0 xs


-- Exercise 3.10
foldLeft :: (b -> a -> b) -> b -> (List a) -> b
foldLeft _ b Empty = b
foldLeft f b (Cons h t) = let b' = b `f` h
                     in seq b' $ foldLeft f b' t

-- Exercise 3.11
sum :: (Num a) => (List a) -> a
sum = foldLeft (\t x -> x + t) 0

-- Exercise 3.11
product :: (Num a) => (List a) -> a
product = foldLeft (\t x -> x * t) 1


-- Exercise 3.11
length2 :: (Num b) => (List a) -> b
length2 = foldLeft (\t x -> t + 1) 0


-- Exercise 3.12
reverse :: (List a) -> (List a)
reverse = foldLeft (\t x -> Cons x t ) (Empty)


-- Exercise 3.13
foldLeftWithFoldRight :: (b -> a -> b) -> b -> (List a) -> b
foldLeftWithFoldRight f b l = foldRight (\x g t -> g ( f t x ) ) id l b

-- Exercise 3.13
foldRightWithFoldLeft :: (a -> b -> b) -> b -> (List a) -> b
foldRightWithFoldLeft f b l = foldLeft (\g x t ->  g(f x t) ) id l b

-- Exercise 3.14
append :: (List a) -> (List a) -> (List a)
append Empty b = b
append a Empty = a
append a b = foldRight (\x t -> Cons x t ) b a


-- Exercise 3.15
concat :: (List(List a)) -> (List a)
concat Empty = Empty
concat xs  = foldRight (\x t -> append x t) Empty xs

-- Exercise 3.16, 3.17 and 3.18
map :: (a -> b) ->(List a) -> (List b)
map f Empty = Empty
map f (Cons h t) = Cons (f h) (map f t)

-- Exercise 3.19
filter :: (a -> Bool) -> (List a) -> (List a)
filter p ls = foldRight (\h t -> if ( p h ) then Cons h t else t  ) Empty ls

-- Exercise 3.20
flatMap :: (a -> List b) -> List a -> List b
flatMap f l = concat (map f l)


-- Exercise 3.21
filterUsingFlatMap  :: (a -> Bool) -> (List a) -> (List a)
filterUsingFlatMap f l = flatMap (\a -> if (f a) then Cons a Empty else Empty) l

-- Exercise 3.22 and 3.23
zipWith :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipWith _ _ Empty = Empty
zipWith _ Empty _ = Empty
zipWith f (Cons h1 t1) (Cons h2 t2) = Cons (f h1 h2) (zipWith f t1 t2)