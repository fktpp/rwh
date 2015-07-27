-- file: ch04/ch04.exercises.hs
import Data.List
import Data.Char (digitToInt, isDigit) 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs
	

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = case safeInit xs of
	Just a	-> Just (x:a)


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = let ( sub, suf ) = break f xs
			in sub : splitWith f ( drop 1 suf )


-- file: ch04/ch04.exercises.hs
addAsInt :: Int -> Char -> Int
addAsInt i c = i * 10 + digitToInt c

asInt_fold :: String -> Int
asInt_fold (x:xs) 
	| x == '-' 		= negate $ asInt_fold' xs
	| otherwise 		= asInt_fold' (x:xs)
	where
		asInt_fold' ys = foldl addAsInt 0 ys

-- file: ch04/ch04.exercises.hs
eaddAsInt :: Int -> Char -> Int
eaddAsInt i c
	| not $ isDigit c				= error "non-digit encounter"
	| i * 10 < i		 			= error "Int overflow when multiplex"
	| maxBound - i * 10 < digitToInt c		= error "Int overflow when plus c"
	| otherwise					= i * 10 + digitToInt c

easInt_fold :: String -> Int
easInt_fold "" = error "Empty string is not supported"
easInt_fold "-" = error "Single negative sign is not supported"
easInt_fold (x:xs) 
	| x == '-' 		= error "negative number is not supported"
	| otherwise 		= easInt_fold' (x:xs)
	where
		easInt_fold' ys = foldl eaddAsInt 0 ys

-- file: ch04/ch04.exercises.hs
type ErrorMessage = String
data Result = Result {
	  digit  :: Int
	, result :: Either ErrorMessage Int
	} deriving (Show)

newacc x y char = digitToInt char * 10 ^ x + y

stepResult _ (Result {digit = d, result = Left e})	= Result { digit = d + 1, result = Left e }
stepResult c r
	| not $ isDigit c		= Result { digit = d + 1, result = Left ("non-digit '" ++ [c] ++ "'") }
	| otherwise			= Result { digit = d + 1, result = Right (newacc d acc c) }
	where
		d = digit r
		Right acc = result r


asInt_either :: String -> Either ErrorMessage Int
asInt_either "" 	= Left "Empty String"
asInt_either "-"	= Left "Empty negative sign"
asInt_either (x:xs)
	| x == '-'	= Left "Negative number is not supported"
	| otherwise	= result $ foldr stepResult (Result {digit=0, result=Right 0}) (x:xs)

-- file: ch04/ch04.exercises.hs
stepList l accl = foldr (:) accl l

myconcat :: [[a]] -> [a]
myconcat xs = foldr stepList [] xs

-- takeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] 	= []
mytakeWhile f (x:xs)
	| f x 		= x : ( mytakeWhile f xs )
	| otherwise	= []

mytakeWhile_foldr :: (a -> Bool) -> [a] -> [a]
mytakeWhile_foldr f = foldr stepf []
	where
		stepf x acc
			| f x 		= (x : acc)
			| otherwise	= []

-- file: ch04/ch04.exercises.hs
mygroupBy_foldr :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupBy_foldr f list = fst $ foldr stepGroup ([], Nothing) list
	where
		stepGroup x (_, Nothing)	= ([[x]], Just x)
		stepGroup x ((y:ys), Just z)
			| f x z			= let newy = x : y in ((newy:ys), Just x)
			| otherwise		= (([x]):(y:ys), Just x)

mygroupBy_foldl :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupBy_foldl f list = foldl stepGroup [] list
	where
		stepGroup [] x 		= [[x]]
		stepGroup xs z
			| f y z			= (oinit ++ [newx])
			| otherwise		= xs ++ [[z]]
			where
				y = last $ last xs
				oinit = init xs
				newx = last xs ++ [z]

{-- above implementation does not pass test: groupBy (<) [1,2,3,2,0,0,3,3,1,0] -}
