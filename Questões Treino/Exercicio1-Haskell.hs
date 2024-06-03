-- Questão 1 --

numVend :: Int -> Int
numVend 0 = 0
numVend 1 = 4
numVend x = 2*x

quantS :: Int -> Int -> Int
quantS s n | n == 0 = 0
           | (numVend n) == s = (1 + quantS s (n-1))
           | otherwise = quantS s (n-1)

-- Questão 2 --

ehDiv :: Int -> Int -> Bool
ehDiv x y | ((mod x y) == 0 && y>1) = True
          | y == 1 = False
          | otherwise = ehDiv x (y-1)

primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo 2 = True
primo x = not (ehDiv x (x-1))

-- Questao 3 -- 

temDiv :: Int -> Int -> Int -> Bool
temDiv x y d | d == 1 = True
             | ((mod x d) == 0) && ((mod y d) == 0) = False
             | otherwise = temDiv x y (d-1)


primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y | x == y = False
                  | temDiv x y (max x y) = True
                  | otherwise = False

-- Questao 4 a --

fat :: Int -> Int 
fat 0 = 1
fat 1 = 1
fat x = x * fat (x-1)

-- Questao 4 b --

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n o p = (m == n) && (n == o) && (o == p)

-- Questao 4 c --

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

all4Equal2 :: Int -> Int -> Int -> Int -> Bool
all4Equal2 a b c d = (allEqual a b c) && (allEqual b c d)

-- Questao 4 d -- 

equalCount :: Int -> Int -> Int -> Int 
equalCount a b c | allEqual a b c = 3
                 | (a /= b) && (b /= c) && (c /= a) = 1
                 | otherwise = 2

