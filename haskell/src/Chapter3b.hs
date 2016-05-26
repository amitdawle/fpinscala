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


fold :: (a -> b) -> (b -> b -> b) -> (Tree a) -> b
fold f g (Leaf a)  = f a
fold f g (Branch l r) = g ( fold f g l ) (fold f g r)

-- max, depth, map and size as fold
maximum2 :: (Ord a) => Tree a -> a
maximum2 = fold id (\x y -> if x > y then x else y)

depth2  :: (Num a, Ord a) => Tree a -> Int
depth2 = fold (\x -> 1) (\x y -> if x > y then x + 1 else y + 1)

map2   :: ( a -> b ) -> (b -> b -> b) ->  Tree a -> b
map2 f g = fold (\v -> f v) (\l r -> g l r)

size2  :: (Num a) => Tree a -> a
size2 = fold (\x -> 1) (\x y -> x + y + 1)