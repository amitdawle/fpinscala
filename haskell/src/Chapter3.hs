module Chapter3 where

data List a = Empty | Cons a (List a) deriving (Eq, Show)

tail :: (Eq a) => (List a) -> (List a)
tail Empty = Empty
tail (Cons _ t) = t