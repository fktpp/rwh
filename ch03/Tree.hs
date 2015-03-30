-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
	      
h Empty = 0
h (Node _ a  b)
	| ha >= hb		= 1 + ha
	| otherwise		= 1 + hb
	where
		ha = h a
		hb = h b
