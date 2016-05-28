module Chapter4 where
import Prelude hiding(map, filter)

data Option a = Some a | None deriving (Show, Eq) -- Maybe a = Just a | Nothing

map :: (a->b) -> Option a -> Option b
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