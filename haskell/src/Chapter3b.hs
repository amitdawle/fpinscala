module Chapter3b where
import Prelude hiding (maximum, map)
import Data.List (inits, tails)


-- Exercise 3.24
hasSubSequence :: (Eq a) => [a] -> [a] -> Bool
hasSubSequence sup sub = sub `elem` (filter ( not . null) . concatMap inits . tails $ sup )


data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)

size :: (Tree a) -> Int
size (Leaf _) = 1
size (Branch l r) = 1 + size l + size r


maximum :: (Ord a, Num a) => (Tree a) -> a
maximum (Leaf v) = v
maximum (Branch l r) = if( lv > rv ) then lv else rv
               where lv = maximum l
                     rv = maximum r


depth :: (Tree a) -> Int
depth (Leaf v) = 1
depth (Branch l r) = if( ld > rd ) then (ld + 1) else (rd + 1)
               where ld = depth l
                     rd = depth r


map :: (a -> b) -> (Tree a) -> (Tree b)
map f (Leaf v) = Leaf (f v)
map f (Branch l r) = Branch (map f l) (map f r)
