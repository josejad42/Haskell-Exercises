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

-- Questao 4 a --

fat :: Int -> Int 
fat 0 = 1
fat 1 = 1
fat x = x * fat (x-1)

-- Questao 4 b --

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal m n o p = (m == n) && (n == o) && (o == p)


-- Questao 4 c --

-- Questao 4 d -- 



