module Week42Exercise1 where
import Data.Either

-- a) implement fromLeftToRight
fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (f . Left, f . Right)

-- b) implement either'
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' a b f =
    case f of
        Left x -> a x
        Right y -> b y

-- c) implement toFstAndSnd
toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

-- d) implement pair
pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)