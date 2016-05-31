module Chapter4b where
import Prelude hiding(map, filter, sequence, Either(Left), Either(Right))

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
