-- lastButOne :: [a] ->  Maybe a

-- lastButOne [] = Nothing
-- lastButOne (x:[]) = Nothing
-- lastButOne (x:y:[]) = Just x
-- lastButOne (x:y:zs) = lastButOne (y:zs)

lastButOne :: [a] ->   a

lastButOne (x:y:[]) = x
lastButOne (x:y:zs) = lastButOne (y:zs)
