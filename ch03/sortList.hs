sortList:: [[a]] -> [[a]]


sortList [] = []
sortList (x:xs) = sortedLeft ++ [x] ++ softedRight
	where
		sortedLeft = sortList [y | y <- xs, (length y) <= (length x) ]
		softedRight = sortList [z | z <-xs, (length z) > (length x) ]
