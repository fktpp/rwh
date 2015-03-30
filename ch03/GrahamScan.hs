import Data.Ord
import Data.List

data Direction = TurnLeft | TurnRight | Straight
				deriving (Show)

data Point = Point {
			x	:: Int
		,	y	:: Int
		} deriving (Show)

gdirection a b c
	| angle > 0	= TurnLeft
	| angle < 0	= TurnRight
	| otherwise	= Straight
	where
		angle = (x b - x a) * ( y c - y a ) - ( y b - y a ) * ( x c - x a )

compare_angle :: Point -> Point -> Point -> Ordering
compare_angle a b c = case gdirection a b c of
				TurnLeft	-> LT
				Straight	-> EQ
				TurnRight	-> GT


r :: [Point] -> [Direction]
r [] = []
r (x:[]) = []
r (x:y:[]) = []
r (x:y:z:zs) = (gdirection x y z) : (r (y:z:zs))

findp :: [Point] -> Point
findp []                = error "Can not find P point from EMPTY list"
findp (x:[])            = x

findp (px:py:[])
    | y px < y py       = px
    | y px == y py      = lower_x_of_px_py
    | otherwise         = py
    where lower_x_of_px_py
              | x px < x py       = px
              | otherwise         = py

findp (x:y:xs)          = findp (( findp (x:y:[]) ) : xs)

dist a b = (x b - x a) ^ 2 + ( y b - y a ) ^ 2

gscan :: [Point] -> [Point]
gscan [] = []

gscan points = scan_base_p point_p sorted_remaining_points
-- gscan points = sorted_remaining_points
			where		
				point_p = findp points
				remaining_points 	= filter (\point_t -> (x point_t) /= (x point_p) || ( y point_t ) /= ( y point_p )) points
				sorted_remaining_points = sortBy (compare_angle point_p) remaining_points

				scan_base_p a [] = []
				scan_base_p a (x:[]) = a:x:[]
				
				scan_base_p a (b:c:xs) = case gdirection a b c of
									TurnLeft	-> a : (scan_base_p b (c:xs))
									TurnRight	-> scan_base_p a (c:xs)
									Straight	-> if (dist point_p b) > (dist point_p c)
												then a : (scan_base_p b xs)
												else  a : (scan_base_p c xs)