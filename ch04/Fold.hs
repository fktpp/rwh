{-
-- file: ch04/Fold.hs
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero


foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)

foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _    zero []     = zero

foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))

-- file: ch04/Fold.hs
 1 : (2 : (3 : []))
 1 + (2 + (3 + 0 ))

filter :: (a -> Bool) -> [a] -> [a]
filter p []   = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-}

-- file: ch04/Fold.hs
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys


-- file: ch04/Fold.hs
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
    where step x ys = f x : ys


-- file: ch04/Fold.hs
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
-- Read
-- http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29

{-
step x g a = g (f a x)
foldr step id (1:2:3:[]) z
      -- first expand foldr
      == step 1                 (foldr step id (2:3:[])) z
      == step 1 (step 2         (foldr step id (3:[])))  z
      == step 1 (step 2 (step 3 (foldr step id [])))     z
      == step 1 (step 2 (step 3 id))                     z
      -- next expand step
      == (step 2 (step 3 id)) (f z 1)
      == step 2 (step 3 id) (f z 1)
      == (step 3 id) (f (f z 1) 2)
      == step 3 id (f (f z 1) 2)
      -- expand id
      == id (f (f (f z 1) 2) 3)
      == f (f (f z 1) 2) 3
-}

-- file: ch04/Fold.hs
identity :: [a] -> [a]
identity xs = foldr (:) [] xs

-- file: ch04/Fold.hs
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
