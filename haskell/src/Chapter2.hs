module Chapter2 ( fib,
                  isSorted )
                  where
-- Exercise 2.1
-- nth Fibonacci number

fib :: Int -> Maybe Int
fib n
  | n <= 0 = Nothing
  | n > 0 = go (n - 1) 0 1
            where go 0 a b = Just a
                  go i a b = go (i - 1) b (b + a)

--    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
isSorted :: [a] -> (a -> a  -> Bool) -> Bool
isSorted [] _ = True
isSorted [x] _ = True
isSorted (x:y:xs) f = f x y && isSorted xs f