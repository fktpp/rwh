-- file: ch04/Fold.hs
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero

foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)

