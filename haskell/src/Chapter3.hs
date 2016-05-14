module Chapter3 where
import Prelude hiding (tail,
                       dropWhile,
                       takeWhile,
                       drop)


data List a = Empty | Cons a (List a) deriving (Eq, Show)


tail :: (List a) -> (List a)
tail Empty = Empty
tail (Cons _ t) = t


setHead :: a -> (List a) -> List a
setHead s Empty = Cons s Empty
setHead s (Cons h t) = Cons s t


drop :: Int -> (List a) -> (List a)
drop _ Empty = Empty
drop n (Cons h t)
     | n <= 0 = (Cons h t)
     | otherwise = drop (n - 1) t


dropWhile :: (a -> Bool) -> (List a) -> (List a)
dropWhile _ Empty = Empty
dropWhile f (Cons h t) = if( f h ) then dropWhile f t else (Cons h t)


takeWhile :: (a -> Bool) -> (List a) -> (List a)
takeWhile _ Empty = Empty
takeWhile f (Cons h t) = if (f h) then (Cons h (takeWhile f t)) else Empty

