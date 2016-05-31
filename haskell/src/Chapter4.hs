module Chapter4 where
import Prelude hiding(map, filter)

data Option a = Some a | None deriving (Show, Eq) -- Maybe a = Just a | Nothing

map :: (a -> b) -> Option a -> Option b
map _ None = None
map f (Some a) = Some (f a)


flatMap :: (a -> Option b) -> Option a -> Option b
flatMap _ None = None
flatMap f (Some a) = f a

getOrElse :: (Option a) -> a -> a
getOrElse None b = b
getOrElse (Some a) _ = a


orElse :: (Option a) -> (Option a) -> (Option a)
orElse None b = b
orElse (Some x) _ = Some x


filter :: (a -> Bool) -> Option a -> Option a
filter _ None = None
filter f (Some x) = if ( f x ) then  Some x else None

-- Exercise 4.2
mean :: (Fractional a) => [a] -> Option a
mean [] = None
mean xs = Some( (sum xs) / fromIntegral (length xs) )

variance :: (Fractional a) => [a] -> Option a
variance xs =  flatMap (\m -> mean [ (m - x) * (m - x)  | x <- xs] )  cm
         where cm = mean xs

-- Exercise 4.3
map2 :: (a -> b -> c) -> (Option a) -> (Option b)  -> Option c
map2 f a b = flatMap (\x -> map (\y -> f x y) b ) a

map2_ :: (a -> b -> c) -> (Option a) -> (Option b)  -> Option c
map2_ _ None  _  = None
map2_ _ _ None   = None
map2_ f (Some a) (Some b) = Some (f a b)

map3 :: (a -> b -> c -> d) -> (Option a) -> (Option b) -> (Option c)  -> Option d
map3 f a b c = flatMap(\x -> map2 (\y z -> f x y z ) b c ) a


-- Exercise 4.4
sequence :: [Option a] -> Option [a]
sequence xs = foldr (map2 (:) ) (Some []) xs
