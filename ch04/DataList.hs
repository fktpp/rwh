elength :: [a] -> Int
elength [] = 0
elength (x:xs) = 1 + ( elength xs )

enull :: [a] -> Bool
enull [] = True
enull _ = False

ehead :: [a] -> a
ehead [] = error "empty list has no head"
ehead (x:_) = x

etail :: [a] -> [a]
etail []  = error "empty list has no tail"
etail (x:xs) = xs

elast :: [a] -> a
elast [] = error "empty list has no last"
elast (x:[])  = x
elast (_:xs) = elast xs

einit :: [a] -> [a]
einit [] = error "empty list has no init"
einit (x:[]) = []
einit (x:xs) = x : (einit xs)

eappend:: [a] -> [a] -> [a]
eappend [] ys = ys
eappend (x:[]) ys = x : ys
eappend (x:xs) ys = x : (eappend xs ys)

econcat:: [[a]] -> [a]
econcat [] = []
econcat (x:xs) = eappend x (econcat xs)

ereverse:: [a] -> [a]
ereverse [] = []
ereverse (x:xs) = eappend (ereverse xs) [x]

eand:: [Bool] -> Bool
eand [] = True
eand (x:xs) = x && eand xs

eor:: [Bool] -> Bool
eor [] = False
eor (x:xs) = x || eor xs

eall :: (a-> Bool) -> [a] -> Bool
eall f xs = eand $ map f xs

eany :: (a -> Bool) -> [a] -> Bool
eany f xs = eor $ map f xs

etake :: Int -> [a] -> [a]
etake _ [] = []
etake 0 _ = []
etake n (x:xs)
	| n1 > 0 		= x : (etake n1 xs)
	| otherwise	= [x]
		where n1 = n - 1

edrop :: Int -> [a] -> [a]
edrop _ [] = []
edrop 0 xs = xs
edrop n (x:xs)
	| n1 >= 0 	= edrop n1 xs
	| otherwise	= (x:xs)
		where n1 = n - 1


esplitAt :: Int -> [a] -> ([a], [a])
-- esplitAt 0 xs = ([], xs)
-- esplitAt _ [] = ([], [])
esplitAt n xs = (etake n xs, edrop n xs)

etakeWhile :: (a -> Bool) -> [a] -> [a]
etakeWhile _ [] = []
etakeWhile f (x:xs)  = case f x of
		True			-> x : (etakeWhile f xs)
		False			-> []

edropWhile :: (a -> Bool) -> [a] -> [a]
edropWhile _ [] = []
edropWhile f (x:xs) = case f x of
		True			-> edropWhile f xs
		False			-> (x:xs)

espan :: (a -> Bool) -> [a] -> ([a], [a])
espan f xs = ( etakeWhile f xs, edropWhile f xs)

ebreak :: (a -> Bool) -> [a] -> ([a], [a])
ebreak f xs = ( etakeWhile notf xs, edropWhile notf xs )
			where notf x = not $ f x

eelem :: (Eq a) => a -> [a] -> Bool
eelem _ [] = False
eelem x1 (x:xs) =  x == x1 || eelem x1 xs

enotElem :: Eq a => a -> [a] -> Bool
enotElem _ [] = True
enotElem x1 (x:xs) = x /= x1 && enotElem x1 xs

efilter :: (a -> Bool) -> [a] -> [a]
efilter _ [] = []
efilter f (x:xs) = case f x of
	True			-> x : (efilter f xs)
	False			-> efilter f xs

eisPrefixOf :: Eq a => [a] -> [a] -> Bool
eisPrefixOf [] _ = True
eisPrefixOf _ [] = False
eisPrefixOf (x:xs) (y:ys) = x == y && eisPrefixOf xs ys

eisInfixOf :: Eq a => [a] -> [a] -> Bool
eisInfixOf [] _ = True
eisInfixOf _ [] = False
eisInfixOf xs (y:ys) = eisPrefixOf xs (y:ys) || eisInfixOf xs ys

eisSuffixOf :: Eq a => [a] -> [a] -> Bool
eisSuffixOf [] _ = True
eisSuffixOf _ [] = False
eisSuffixOf xs ys = let rxs = ereverse xs
			rys = ereverse ys
		    in eisPrefixOf rxs rys

ezip :: [a] -> [b] -> [(a, b)]
ezip _ [] = []
ezip [] _ = []
ezip (x:xs) (y:ys) = (x,y) : ezip xs ys

ezipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
ezipWith _ _ [] = []
ezipWith _ [] _ = []
ezipWith f (x:xs) (y:ys) = f x y : ezipWith f xs ys

elines :: String -> [String]
elines "" = []
elines cs = 
    let (pre, suf) = ebreak isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> elines rest
                ('\r':rest)      -> elines rest
                ('\n':rest)      -> elines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

eunlines :: [String] -> String
eunlines [] = ""
eunlines (x:xs) = x `eappend` [ '\n' ] `eappend` (eunlines xs)

isWhiteSpace c = case c of
	'\r'	-> True
	'\n'	-> True
	' '	-> True
	'\t'	-> True
	_ 	-> False

ewords :: String -> [String]
ewords "" = []
ewords cs = pre : ewords striped_suf
	where
		(pre, suf ) = ebreak isWhiteSpace cs
		
		striped_suf = edropWhile isWhiteSpace suf

eunwords :: [String] -> String
eunwords [] = ""
eunwords (x:xs) = x `eappend` [ ' ' ] `eappend` (eunwords xs)
