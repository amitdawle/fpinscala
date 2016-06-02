module Chapter4b where
import Prelude hiding(map, filter, sequence, Either(Left), Either(Right), traverse)

data Either e a = Left e | Right a deriving (Show, Eq) -- Maybe a = Just a | Nothing

map :: (a -> b) -> Either e a -> Either e b
map _ (Left e) = (Left e)
map f (Right a) = Right (f a)

flatMap :: (a -> Either e b) -> Either e a -> Either e b
flatMap _ (Left e) = (Left e)
flatMap f (Right a) = f a

orElse :: (Either e a) -> (Either e a) -> (Either e a)
orElse (Left _) x = x
orElse (Right a) _ = (Right a)

map2 :: (a -> b -> c)  -> (Either e a) -> (Either e b) -> Either e c
map2 f a b = flatMap (\x -> map (\y -> f x y) b ) a

sequence :: [Either e a] -> Either e [a]
sequence  = foldr (\x t -> map2 (:) x t ) (Right [])

traverse :: (a -> Either e b) -> [a] -> Either e [b]
traverse f = foldr (\x t -> map2 (:) (f x) t ) (Right [])

sequence2 :: [Either e a] -> Either e [a]
sequence2  = traverse id

-- Exercise 4.8
map22 :: (a -> b -> c)  -> (Either e a) -> (Either [e] b) -> Either [e] c
map22 _ (Left a) (Left b) = Left (a:b)
map22 _ _ (Left b) = Left b
map22 _ (Left a) _ = Left (a:[])
map22 f (Right a) (Right b) = Right (f a b)


sequence22 :: [Either e a] -> Either [e] [a]
sequence22  = foldr (\x t -> map22 (:) x t ) (Right [])