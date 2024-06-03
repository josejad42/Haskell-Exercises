fatPrime :: Int -> [(Int, Int)]
fatPrime x = fatPrimeAux x x []


fatPrimeAux :: Int -> Int -> [(Int, Int)]-> [(Int, Int)]
fatPrimeAux x d t | d == 1 = t
                  | fat x d == 0 = fatPrimeAux x (d-1) t
                  | ((fat x d) > 0) && (primo d) = fatPrimeAux x (d-1) ((d,(fat x d)) : t)
                  | otherwise = fatPrimeAux x (d-1) t

ehDiv :: Int -> Int -> Bool
ehDiv x y | ((mod x y) == 0 && y>1) = True
          | y == 1 = False
          | otherwise = ehDiv x (y-1)

primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo 2 = True
primo x = not (ehDiv x (x-1))

fat :: Int -> Int -> Int 
fat x d | x == 1 = 0
        | (mod x d) == 0 = 1 + (fat (div x d ) d)
        | otherwise = 0 

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result