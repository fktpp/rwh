-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)


splitLines :: String -> [String]

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

concatChar :: Char -> Char -> String
concatChar a b = a : b : []

isNotWhite :: Char -> Bool
isNotWhite x = case x of
	'\r' 		-> False
	'\n'		-> False
	' '		-> False
	'\t'		-> False
	_		-> True

mapZipWith:: (Char -> Char -> String) -> [String] -> [String]
mapZipWith _ [] = []
mapZipWith _ (x:[]) = []
mapZipWith zipper (x:y:zs) = zipWith zipper non_white_x non_white_y ++ mapZipWith zipper zs
					where
						non_white_x = filter isNotWhite x
						non_white_y = filter isNotWhite y

fixLines :: String -> String
fixLines input = unlines zippedLines
	where
		zippedLines = mapZipWith concatChar ( splitLines input )

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = fixLines