import Data.Maybe;
import Data.List;

-- 1:
maybeFunc :: (a -> b) -> Maybe a -> Maybe a -> Maybe a -> Maybe [b]
maybeFunc f a b c = if isNothing a || isNothing b || isNothing c
    then Nothing
    else maybeFunc' f a b c where
        maybeFunc' f (Just a) (Just b) (Just c) = Just [f a, f b, f c]

-- 2:
maybeFuncList :: (a -> b) -> [Maybe a] -> Maybe [b]
maybeFuncList f lst = if hasNothing lst then Nothing
    else Just (map (f . fromJust) lst) where
        hasNothing lst = not $ isNothing $ find isNothing lst

-- 3:
data Peano = Zero | Succ Peano deriving Show

add :: Peano -> Peano -> Peano
add a Zero = a
add a (Succ b) = add (Succ a) b

sub :: Peano -> Peano -> Peano
sub a Zero = a
sub Zero _ = error "a < b"
sub (Succ a) (Succ b) = sub a b

mult :: Peano -> Peano -> Peano
mult a b = mult' a b a where
    mult' a Zero _ = Zero
    mult' Zero a _ = Zero
    mult' a (Succ Zero) _ = a
    mult' (Succ Zero) a _ = a
    mult' a (Succ b) aa = mult' (add a aa) b aa

-- 4:
data List a = Nil | Node a (List a) (List a) deriving Show

moveNext :: List a -> List a
moveNext Nil = Nil
moveNext (Node a prev next) = next

movePred :: List a -> List a
movePred Nil = Nil
movePred (Node a prev next) = prev