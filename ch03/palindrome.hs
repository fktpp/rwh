myr [] = []
myr (x:xs) = (myr xs) ++ [x]

pdList []  = []
pdList x = x ++ (myr x)

ispdList x
	| odd $ length x		= False
	| x /= myr x 		= False
	| otherwise		= True

