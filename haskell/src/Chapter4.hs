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
map2 :: (Option a) -> (Option b) -> (a -> b -> c) -> Option c
map2 a b f = flatMap (\x -> map (\y -> f x y) b ) a

map2_ :: (Option a) -> (Option b) -> (a -> b -> c) -> Option c
map2_ None _ _  = None
map2_ _ None _  = None
map2_ (Some a) (Some b) f  = Some (f a b)

map3 :: (Option a) -> (Option b) -> (Option c) -> (a -> b -> c -> d) -> Option d
map3 a b c f = flatMap( \x -> flatMap (\y -> map (\z -> f x y z) c ) b ) a




