module Chapter3 where
import Prelude hiding (tail,
                       dropWhile,
                       takeWhile,
                       drop,
                       init)


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

