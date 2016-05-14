module Chapter2 ( fib,
                  isSorted,
                  curry,
                  uncurry,
                  compose)
                  where
import Prelude hiding(curry, uncurry, compose)


-- Exercise 2.1
-- nth Fibonacci number
fib :: Int -> Maybe Int
fib n
  | n <= 0 = Nothing
  | n > 0 = go (n - 1) 0 1
            where go 0 a b = Just a
                  go n a b = go (n - 1) b (b + a)


-- Exercise 2.2
--    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
isSorted :: [a] -> (a -> a  -> Bool) -> Bool
isSorted [] _ = True
isSorted [x] _ = True
isSorted (x:y:xs) f = f x y && isSorted (y:xs) f


-- Exercise 2.3
-- def curry[A, B, C](f: (A, B) => C) : A => (B => C)
-- In Haskell, functions are curried by default hence the following is bit synthetic
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \a ->(\b -> f(a , b) )


-- Exercise 2.4
--  def uncurry[A, B, C](f: A => B => C ) : (A, B) => C = (a, b) => f(a)(b)
uncurry :: ( a -> ( b -> c)) -> ( (a, b) -> c)
uncurry f = \(a, b) -> (f a) b


-- Exercise 2.5 compose
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> f ( g a )