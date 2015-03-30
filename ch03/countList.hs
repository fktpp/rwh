countList:: [a] -> Int

countList [] = 0
countList (_:xs) = 1 + ( countList xs )


listMean [] = 0
listMean xs = fsum / fcount
		where
			fsum = 	fromIntegral $ listSum xs
				where
					listSum [] = 0
					listSum (y:ys) = y + ( listSum ys)

			fcount = fromIntegral $ countList xs

