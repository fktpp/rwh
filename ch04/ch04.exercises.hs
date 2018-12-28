-- file: ch04/ch04.exercises.hs
module Test where

import Data.List()
import Data.Char (digitToInt, isDigit)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs


safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = case safeInit xs of
        Just a  -> Just (x:a)


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = let ( sub, suf ) = break f xs
                        in sub : splitWith f ( drop 1 suf )


-- file: ch04/ch04.exercises.hs
addAsInt :: Int -> Char -> Int
addAsInt i c = i * 10 + digitToInt c

asIntFold :: String -> Int
asIntFold (x:xs)
        | x == '-'              = negate $ asIntFold' xs
        | otherwise             = asIntFold' (x:xs)
        where
                asIntFold' = foldl addAsInt 0

-- file: ch04/ch04.exercises.hs
eaddAsInt :: Int -> Char -> Int
eaddAsInt i c
        | not $ isDigit c                               = error "non-digit encounter"
        | i * 10 < i                                    = error "Int overflow when multiplex"
        | maxBound - i * 10 < digitToInt c              = error "Int overflow when plus c"
        | otherwise                                     = i * 10 + digitToInt c

easIntFold :: String -> Int
easIntFold "" = error "Empty string is not supported"
easIntFold "-" = error "Single negative sign is not supported"
easIntFold (x:xs)
        | x == '-'              = error "negative number is not supported"
        | otherwise             = easIntFold' (x:xs)
        where
                easIntFold' = foldl eaddAsInt 0

-- file: ch04/ch04.exercises.hs
type ErrorMessage = String
data Result = Result {
          digit  :: Int
        , result :: Either ErrorMessage Int
        } deriving (Show)

newacc x y char = digitToInt char * 10 ^ x + y

stepResult _ Result{digit = d, result = Left e}      = Result { digit = d + 1, result = Left e }
stepResult c r
        | not $ isDigit c               = Result { digit = d + 1, result = Left ("non-digit '" ++ [c] ++ "'") }
        | otherwise                     = Result { digit = d + 1, result = Right (newacc d acc c) }
        where
                d = digit r
                Right acc = result r


asIntEither :: String -> Either ErrorMessage Int
asIntEither ""         = Left "Empty String"
asIntEither "-"        = Left "Empty negative sign"
asIntEither (x:xs)
        | x == '-'      = Left "Negative number is not supported"
        | otherwise     = result $ foldr stepResult Result{digit = 0, result = Right 0} (x:xs)

-- file: ch04/ch04.exercises.hs
stepList l accl = foldr (:) accl l

myconcat :: [[a]] -> [a]
myconcat = foldr stepList []

-- takeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ []        = []
mytakeWhile f (x:xs)
        | f x           = x : mytakeWhile f xs
        | otherwise     = []

mytakeWhileFoldr :: (a -> Bool) -> [a] -> [a]
mytakeWhileFoldr f = foldr stepf []
        where
                stepf x acc
                        | f x           = x : acc
                        | otherwise     = []

-- file: ch04/ch04.exercises.hs
mygroupByFoldr :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupByFoldr f list = fst $ foldr stepGroup ([], Nothing) list
        where
                stepGroup x (_, Nothing)        = ([[x]], Just x)
                stepGroup x (y : ys, Just z)
                        | f x z                 = let newy = x : y in (newy : ys, Just x)
                        | otherwise             = ([x]:(y:ys), Just x)

mygroupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupByFoldl f  = foldl stepGroup []
        where
                stepGroup [] x          = [[x]]
                stepGroup xs z
                        | f y z                 = oinit ++ [newx]
                        | otherwise             = xs ++ [[z]]
                        where
                                y = last $ last xs
                                oinit = init xs
                                newx = last xs ++ [z]

{-- above implementation does not pass test: groupBy (<) [1,2,3,2,0,0,3,3,1,0] -}

myanyFoldr :: Foldable t => (a -> Bool) -> t a -> Bool
myanyFoldr f  = foldr orfunc False
        where
                orfunc a b = f a || b

myanyFoldl :: Foldable t => (a -> Bool) -> t a -> Bool
myanyFoldl f  = foldl orfunc False
    where
        orfunc a b = a || f b

-- myCycleFoldr :: [a] -> [a]
-- myCycleFoldl :: [a] -> [a]

-- myWordsFoldr :: String -> [String]
-- myWordsFoldl :: String -> [String]

-- myUnlinesFoldr :: [String] -> String
-- myUnlinesFoldl :: [String] -> String

