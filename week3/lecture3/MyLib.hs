module MyLib where

{- map, filter, foldl, and foldr already covered in previous lecture -} 


{- Polymorphic data-types -}

{- A polymorphic list data-type -}
data List t = Nil
            | Cons t (List t) -- similar to coq type declaration syntax

xs :: [Integer]
xs = [1, 2, 3]

ys :: [Char]
ys = ['a', 'n', 'a']


{- Polymorphic functions -} 

{- Non-tail-recursive implementation of a polymorphic filter function -}
{- filterList :: ( t -> Bool) -> List t -> List t -}

filterList f Nil = Nil
filterList f (Cons x xs)
  | f x = Cons x (filterList f xs)
  | otherwise = filterList f xs 

{- Non-tail-recursive implementation of a polymorphic map function -}
{- mapList :: (t1 -> t2) -> List t1 -> List t2 -} 
mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)



{- Read the prelude -}

{- Haskell's option type : the Maybe type -}
-- data Maybe a = Nothing | Just a; we comment this out because it is already included
-- in the prelude 

{- Use the Maybe type to deal with partial functions (not to be confused with partially
applied functions), e.g., head -}

safeHead :: [t] -> Maybe t
safeHead [] = Nothing
safeHead (x : xs) = Just x








