square :: Int -> Int 
square x = x * x  

allEqual :: Int -> Int -> Bool
allEqual a b = (a == b)

maxi :: Int -> Int -> Int
maxi a b | a >= b = a 
         | otherwise = b

 -- casamento de padrões 
vendas :: Int -> Int
vendas 0 = 0 
vendas 1 = 13
vendas 2 = 20 
vendas 3 = 12
vendas 4 = 7
vendas 5 = 14 

totalVendas :: Int -> Int
totalVendas n | n == 0  = vendas 0 
              |otherwise = vendas n + totalVendas (n-1) 

maxiVendas :: Int -> Int 
maxiVendas n | n == 0 = vendas 0 
            | otherwise = maxi (vendas n) (maxiVendas (n-1)) 
fat:: Int -> Int 
fat n | n == 0 = 1
      | otherwise = n * fat (n-1)
--all4Equal:: Int -> Int -> Int -> Int -> Int
--all4Equal a b c d | a == b && a == c && a == d = 4  
--                  | otherwise = maxi (maxi (maxi4Equal a b c d) (maxi4Equal b a c d)) (maxi (maxi4Equal c a b d) (maxi4Equal d a c b)) 
boolInInt:: Bool -> Int
boolInInt value | value == True = 1 
                | otherwise = 0
contEqual :: Int -> Int -> Int -> Int -> Int 
contEqual a b c d = (boolInInt(allEqual a b) + boolInInt(allEqual a c) + boolInInt(allEqual a d)) 
all4Equal :: Int -> Int -> Int -> Int -> Int
all4Equal x1 x2 x3 x4 | (contEqual x1 x2 x3 x4) > 0 = contEqual x1 x2 x3 x4 + 1 
                      | otherwise = contEqual x1 x2 x3 x4 

--Definições Locais 
sumSquares :: Int -> Int -> Int
sumSquares x y = sqx + sqy
    where sqx = x * x
          sqy = y * y

mediaVendas :: Int -> Float
mediaVendas n = fromIntegral(totalVendas n) / fromIntegral n
--(div n):r

--imprimeSemanas :: Int -> [IO()]
--imprimeSemanas n = putStrLn(show n) imprimeSemanas (n - 1)

--printVendas :: Int -> String
--printVendas n | (n == 0) = show(vendas 0)
--              | other
--imprimeSemanas :: Int->String
--imprimeSemanas n = show(vendas)
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

double :: [Int] -> [Int]
double [] = [] 
double (a:as) = [a+a] ++ double as 

member :: [Int] -> Int -> Bool
member [] x = False
member (a:as) x | x == a = True
                | otherwise = member as x
digits :: String -> String
digits [] = []
digits (c:cs) | (c >= '0') && (c <= '9') = c:digits cs
              |otherwise = digits cs