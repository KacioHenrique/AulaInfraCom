data List t = Nil | Cons t (List t)
    deriving(Show)
data Tree t = Nilt | Node t (Tree t) (Tree t)
    deriving(Show)
toList :: List t -> [t]
toList Nil = []
toList (Cons x y) = (x:(toList y))

fromList :: [t] -> List t
fromList [] = Nil 
fromList (a:as) = (Cons a (fromList as)) 

depth :: Tree t -> Int
depth Nilt = 0
depth (Node t tl tr) = depth tl + depth tr + 1

collapse :: Tree t -> [t]
collapse Nilt = []
collapse (Node t tl tr) = [t] ++ (collapse tl) ++ (collapse tr)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ Nilt = Nilt
mapTree f (Node t tl tr) = (Node (f t) (mapTree f tl) (mapTree f tr))


double:: Int -> Int 
double n = n * 2

--xxx yy w zz
--decode :: String -> String
--decode [] = []
--decode (a:as) = a:(show(contVariavel a as)) ++ decode as

--[[x,x,x],[y,y],[z,z]]
--tuplaDecode::String -> [String]
--tuplaDecode 



espaceString::String -> String 
espaceString [] = []
espaceString (a:[]) = [a]
espaceString (a:a2:[]) = [a2]
espaceString (a:a2:as) | (a == a2) = [a] ++ [a2] ++ espaceString as 
                       | otherwise = [a] ++ " " ++ [a2] ++ espaceString as
